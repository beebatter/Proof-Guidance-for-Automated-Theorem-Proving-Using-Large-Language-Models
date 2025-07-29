## Authors: Jelle Piepenbrock, Miroslav Olšák, Josef Urban
## This file is mainly a PyTorch reimplementation by Jelle Piepenbrock of the TensorFlow 1 code written by
## Miroslav Olšák (used for the paper "Property Invariant Embedding for Automated Reasoning" , with some extra tricks
## such as LayerNormalization.

## The fcoplib library used to parse the files was written by Cezary Kaliszyk.



import os
from piegnn_config import LAYERS, EXPNAME, DIMENSIONS, LEARNING_RATE, GPU
# os.environ["CUDA_VISIBLE_DEVICES"] = f"{GPU}"
import random
import numpy as np
import fcoplib as cop
import torch
torch.set_num_threads(1)
#torch.set_num_interop_threads(1)
import torch.nn as nn
from torch_scatter import segment_csr
from graph_data import GraphData

def load_data(datadir, sample=-1):
    fnames = os.listdir(datadir)
    import random

    if not sample == -1:
        if sample > len(fnames):
            sample = len(fnames)
        fnames = random.sample(fnames, sample)

    print(f"Number of files to load: {len(fnames)}")
    test = False
    if not test:
        random.shuffle(fnames)

    data_list = []
    for fname in fnames:
        print(fname)
        data, (lens, labels, symbols) = cop.load_premsel(os.path.join(datadir, fname))
        # print(symbols)
        data_list.append((GraphData(data), (lens, labels)))

    return data_list

def segment_reduce(data, agg_segments, device):
    # We add as a last number to the segments, the length of the data (segment_csr is different from the tensorflow
    # segment operation
    agg_segments = torch.cat(
        (agg_segments, torch.tensor(data.shape[0]).reshape(1).to(device))
    )

    reduced_max = segment_csr(data, agg_segments, reduce="max")
    reduced_mean = segment_csr(data, agg_segments, reduce="mean")

    cat = torch.cat((reduced_max, reduced_mean), axis=1)

    return cat


def segment_reduce_prime(data, agg_segments, device):

    agg_segments = torch.cat(
        (agg_segments, torch.tensor(data.shape[0]).reshape(1).to(device))
    )

    reduced_max = segment_csr(data, agg_segments, reduce="max")
    reduced_min = segment_csr(data, agg_segments, reduce="min")
    reduced_mean = segment_csr(data, agg_segments, reduce="mean")

    cat = torch.cat((reduced_mean, reduced_max + reduced_min), axis=1)

    return cat


def gather_opt(vectors, indices, device):
    """"Prepend another zero vector to the node embeddings, so the index 0 will refer to that 0 embedding"""
    # print("inside gather opt")
    zeros = torch.zeros(vectors.shape[1:]).to(
        device
    )  # we are getting a batch of vectors so 0 dim is batch dim

    vectors = torch.cat((zeros.unsqueeze(0), vectors))

    if indices.dtype == torch.float64:
        indices = indices.long()

    # Remember that we gave non-existent nodes the label -1, so they will index to the 0 element
    return vectors[indices + 1]


def exclusive_cumulative_sum(tens):
    """PyTorch does not have exclusive cumulative sum, so implementing it by cumsum and roll"""

    cumulative_sum = tens.cumsum(0)
    rolled_cumulative_sum = cumulative_sum.roll(
        1, 0
    )  # roll 1 position to the right on axis 0)
    ex_cumsum = rolled_cumulative_sum.clone()
    ex_cumsum[0] = 0
    return ex_cumsum


def prepare_segments(lens):

    # Remember where the nonzero segments (nodes with no incoming messages) are so we know where to pad with empty
    # messages later
    nonzero_indices = torch.nonzero(lens).reshape(-1)

    # Remove the empty segments (nodes with no incoming messages)
    nonzero_lens = lens[nonzero_indices].reshape(-1)

    # Get the segment divider indices for the segment_mean/max ops
    segments = exclusive_cumulative_sum(nonzero_lens)

    return nonzero_indices, nonzero_lens, segments


def add_zeros(data, nonzero_indices, full_shape, device):
    """We add a 0 vector at places where there is a node that got no incoming messages"""

    # Make zero vector
    zero_base_tensor = torch.zeros(full_shape).to(device)

    # Put the non-zero data in the right place, replacing the zero vectors
    zero_base_tensor.index_copy_(0, nonzero_indices, data)

    # We have added 0 for the nodes that did not have inputs (so the shape will stay the same)
    return zero_base_tensor


def combine_graphs(batch):
    """Takes the functionality originally implemented in GraphPlaceholder's feed function"""

    batch = [g.clone() for g in batch]
    node_nums = [g.num_nodes for g in batch]
    symbol_nums = [g.num_symbols for g in batch]
    clause_nums = [g.num_clauses for g in batch]

    data = GraphData.ini_list()
    for g in batch:
        data.append(g)
    data.flatten()

    return data, (node_nums, symbol_nums, clause_nums)


def batch_graphs(data):
    graphs, label_infos = zip(*data)
    # print(label_infos)
    # Make all of the graphs 1 big graph
    big_graph, segments = combine_graphs(graphs)
    # print(segments)
    return (big_graph, segments), label_infos


class PIEGNN(nn.Module):
    """PyTorch implementation of the network from Property Invariant Embedding (PIE) for Automated Reasoning"""

    def __init__(
        self,
        start_dims,
        next_dims,
        hidden_dim=8,
        layers=2,
        repeat_layer=False,
        residual=True,
        normalization="batch",
        pool_proc_id=0
    ):

        #print(torch.__config__.parallel_info(), flush=True)
        
        super(PIEGNN, self).__init__()

        # Whether we use one layer that we repeat or separate parameters per layer / hop
        self.repeat_layer = repeat_layer

        self.residual = residual
        self.num_layers = layers
        self.normalization = normalization

        self.hidden_dim = hidden_dim
        self.node_dim, self.symbol_dim, self.clause_dim = start_dims
        self.node_dim_next, self.symbol_dim_next, self.clause_dim_next = next_dims

        # Initialize embeddings
        num_visible_gpus = torch.cuda.device_count()

        if num_visible_gpus:
            gpu_id = pool_proc_id % num_visible_gpus
            self.device = f"cuda:{gpu_id}"
        else:
            #gpu_id = 0
            self.device = "cpu"
        #self.device = f"cuda:{gpu_id}"
        # self.device = "cuda:0"

        from torch.nn.init import xavier_uniform_

        dim_nodes, dim_symbols, dim_clauses = (
            self.node_dim,
            self.symbol_dim,
            self.clause_dim,
        )

        # Default tensorflow initialization is glorot uniform, so we use that too
        self.node_emb = nn.Parameter(xavier_uniform_(torch.empty(4, dim_nodes)), requires_grad=True)
        self.symbol_emb = nn.Parameter(xavier_uniform_(torch.empty((2, dim_symbols))), requires_grad=True)
        self.clause_emb = nn.Parameter(xavier_uniform_(torch.empty((3, dim_clauses))), requires_grad=True)

        # Several normalizations
        if self.normalization == "batch":
            self.clause_bn = nn.BatchNorm1d(self.clause_dim_next)
            self.node_bn = nn.BatchNorm1d(self.node_dim_next)
            self.symbol_bn = nn.BatchNorm1d(self.symbol_dim_next)
        elif self.normalization == "layer":
            self.clause_ln = nn.LayerNorm(self.clause_dim_next)
            self.node_ln = nn.LayerNorm(self.node_dim_next)
            self.symbol_ln = nn.LayerNorm(self.symbol_dim_next)

        # We need to specify modulelists so we can easily find our multiple layers

        self.mc_list = nn.ModuleList()
        self.mct_list = nn.ModuleList()
        self.mts_123_list = nn.ModuleList()
        self.ms_list = nn.ModuleList()
        self.mts_bold_list = nn.ModuleList()
        self.bst_list = nn.ParameterList()

        self.node_processor_layer_list = (
            nn.ModuleList()
        )  # double list, with n-layer elements with sublist of size 3
        self.symbol_processor_layer_list = nn.ModuleList()  # same but for the symbols
        self.y_aggregator_list = (
            nn.ModuleList()
        )  # double list again with 3 in each layer

        self.mtc_list = nn.ModuleList()
        self.mt_list = nn.ModuleList()

        for layer in range(0, self.num_layers):
            # First layer (initial embedding) can have different size but afterwards all need to be the same for residuals
            if layer == 0:
                clause_in = self.clause_dim
                clause_out = self.clause_dim_next

                node_in = self.node_dim
                node_out = self.node_dim_next

                symbol_in = self.symbol_dim
                symbol_out = self.symbol_dim_next

            else:
                clause_in = self.clause_dim_next
                clause_out = self.clause_dim_next

                node_in = self.node_dim_next
                node_out = self.node_dim_next

                symbol_in = self.symbol_dim_next
                symbol_out = self.symbol_dim_next

            self.mc_list.append(nn.Linear(clause_in, clause_out))

            self.mct_list.append(nn.Linear(2 * node_in, clause_out, bias=False))
            self.mts_123_list.append(nn.Linear(3 * node_in, symbol_out, bias=True))
            self.ms_list.append(nn.Linear(symbol_in, symbol_out, bias=False))
            self.mts_bold_list.append(nn.Linear(2 * symbol_out, symbol_out, bias=False))
            self.bst_list.append(nn.Parameter(torch.randn(size=(1, node_out))))

            # We will add a processorlist for each layer
            self.node_processor_layer_list.append(nn.ModuleList())
            # Then we will add three networks to this modulelist
            for i in range(3):
                self.node_processor_layer_list[layer].append(
                    nn.Linear(2 * node_in, node_out, bias=False)
                )

            # same idea as above:
            self.symbol_processor_layer_list.append(nn.ModuleList())
            for i in range(3):
                self.symbol_processor_layer_list[layer].append(
                    nn.Linear(symbol_in, node_out, bias=False)
                )

            self.y_aggregator_list.append(nn.ModuleList())
            for i in range(3):
                self.y_aggregator_list[layer].append(
                    nn.Linear(2 * node_out, node_out, bias=False)
                )

            self.mtc_list.append(nn.Linear(2 * clause_in, node_out, bias=False))

            self.mt_list.append(nn.Linear(node_in, node_out))

        # 4 times the clause dimension as input because we concat the conjecture to each clause
        # And for both of them we use both max and mean
        # There is only 1 of these, not 1 for every layer
        self.clause_decider = nn.Sequential(
            nn.Linear(4 * self.clause_dim_next, self.hidden_dim),
            nn.ReLU(),
            nn.Linear(self.hidden_dim, 1),
        )

        self.to(self.device)

    def graph_start(self, graph_object):
        # print("inside graph start")
        # print(graph_object)
        # print("aas")
        ini_nodes = torch.tensor(graph_object.ini_nodes)
        ini_symbols = torch.tensor(graph_object.ini_symbols)
        ini_clauses = torch.tensor(graph_object.ini_clauses)
        # print("ini success")
        nodes = self.node_emb[ini_nodes]
        symbols = self.symbol_emb[ini_symbols]
        clauses = self.clause_emb[ini_clauses]
        # print("embedding generated")
        return nodes, symbols, clauses

    def graph_conv(self, node_vectors, graph_structure, layer):
        # print("we are in graph conv")
        nodes, symbols, clauses = node_vectors
        # print("vectors unzi")
        in_nodes = []

        for e, n in enumerate(graph_structure.node_inputs):
            # print("inside for loop")
            if not len(n.nodes) == 0:
                # print("there are terms")
                vector_representations = gather_opt(nodes, torch.tensor(n.nodes), self.device)
                # print("gathered")
                dim = vector_representations.shape[1] * vector_representations.shape[2]
                # print("dim")

                vector_representations = torch.reshape(
                    vector_representations, [-1, dim]
                )
                # print("rehaped")
                xn = self.node_processor_layer_list[layer][e].forward(
                    vector_representations
                )
                # print("nodes processed")
                xs = symbols[torch.tensor(n.symbols, dtype=torch.long)]
                # print("symbols selected")
                xs = xs * torch.tensor(n.sgn).unsqueeze(1).to(self.device)

                # print("rescaling by sign")

                # _ = self.symbol_processor_layer_list[layer][e]
                # print("indexing works")
                # print(type(xs))
                # print(xs.shape)
                # print(self.symbol_dim)
                # try:

                ###xs = self.symbol_processor_layer_list[layer][e].forward(torch.tensor(xs, dtype=torch.float))
                xs = self.symbol_processor_layer_list[layer][e].forward(xs.float())

                # except Exception as e:
                #     print(e)
                # print("symbols processed")
                nonzero_indices, nonzero_lens, segments = prepare_segments(
                    torch.tensor(n.lens)
                )

                zijd = segment_reduce(
                    torch.relu(xn + xs + self.bst_list[layer]), segments.to(self.device), self.device
                )

                # Like in Mirek's implementation, we actually add the zeros and then apply the MLP, is that
                # Useful however? There is no bias in this layer, so the answer will always be 0 too - perhaps
                # We could optimize a bit here

                zijd = add_zeros(
                    zijd, nonzero_indices.to(self.device), (nodes.shape[0], zijd.shape[1]), self.device
                )
                zijd = self.y_aggregator_list[layer][e].forward(zijd)
                in_nodes.append(zijd)
            # print(f"node inuts{e}")
        # Add tij
        node_representations = self.mt_list[layer](nodes)

        in_nodes.append(node_representations)

        # out_nodes <- clauses

        nc = graph_structure.node_c_inputs
        x = clauses[torch.tensor(nc.data)]

        nonzero_indices, nonzero_lens, segments = prepare_segments(
            torch.tensor(nc.lens)
        )

        x = segment_reduce(x, segments.to(self.device), self.device)

        x = add_zeros(x, nonzero_indices.to(self.device), (nodes.shape[0], x.shape[1]), self.device)
        vij = self.mtc_list[layer](x)

        in_nodes.append(vij)

        # out_symbols <- symbols, nodes
        sy = graph_structure.symbol_inputs
        x = gather_opt(nodes, torch.tensor(sy.nodes), self.device)
        dim = x.shape[1] * x.shape[2]
        x = torch.reshape(x, [-1, dim])
        x = self.mts_123_list[layer](x)

        x = x * torch.tensor(sy.sgn).unsqueeze(1).to(self.device)
        segments = exclusive_cumulative_sum(torch.tensor(sy.lens))
        x = segment_reduce_prime(x, segments.to(self.device), self.device)

        symbol_next_first_term = self.ms_list[layer](symbols)

        ###symbol_next_second_term = self.mts_bold_list[layer](torch.tensor(x, dtype=torch.float))
        symbol_next_second_term = self.mts_bold_list[layer](x.float())

        out_symbols = torch.tanh(symbol_next_first_term + symbol_next_second_term)

        # out_clauses <- nodes, clauses

        c = graph_structure.clause_inputs
        x = nodes[torch.tensor(c.data)]
        segments = exclusive_cumulative_sum(torch.tensor(c.lens))
        x = segment_reduce(x, segments.to(self.device), self.device)

        clause_next_first_term = self.mc_list[layer](clauses)

        clause_next_second_term = self.mct_list[layer](x)

        out_clauses = torch.relu(clause_next_first_term + clause_next_second_term)

        out_nodes = in_nodes[0]
        for term in in_nodes[1:]:
            out_nodes = out_nodes + term

        return out_nodes, out_symbols, out_clauses

    def forward(
        self,
        graph_structure,
        problem_lens,
        clause_segments,

    ):
        # print("We are inside forward")
        # Initialize all the embeddings
        # print(graph_structure)
        # print("printing complete")
        vector_representations = self.graph_start(graph_structure)
        # print("got vector")

        for i in range(self.num_layers):
            # print(f"Executing layer {i}")
            if i == 0:
                nodes, symbols, clauses = self.graph_conv(
                    vector_representations, graph_structure, i
                )

            else:
                if self.repeat_layer:
                    new_nodes, new_symbols, new_clauses = self.graph_conv(
                        (nodes, symbols, clauses), graph_structure, 0
                    )

                else:
                    new_nodes, new_symbols, new_clauses = self.graph_conv(
                        (nodes, symbols, clauses), graph_structure, i
                    )

                # residual_nodes, residual_symbols, residual_clauses = vector_representations
                if self.residual:
                    nodes = nodes + new_nodes
                    symbols = symbols + new_symbols
                    clauses = clauses + new_clauses
                # elif not self.residual and self.normalize()
                else:
                    nodes = new_nodes
                    symbols = new_symbols
                    clauses = new_clauses

                if self.normalization == "batch":
                    nodes = self.node_bn(nodes)
                    symbols = self.symbol_bn(symbols)
                    clauses = self.clause_bn(clauses)
                elif self.normalization == "layer":
                    nodes = self.node_ln(nodes)
                    symbols = self.symbol_ln(symbols)
                    clauses = self.clause_ln(clauses)

        # Premises and conjectures can consist of multiple clause nodes
        # We have to collapse them
        # For this we have clause_segments, which is a list of the length (no. of clauses) for each premise and the
        # conjecture

        # print(clause_segments)
        clause_segment_lens = [len(k) for k in clause_segments]
        clause_segments = [torch.tensor(k) for k in clause_segments]
        # print(clause_segment_lens)
        # assert 2 > 3
        # clause_segment_lens = [k[2] for k in problem_lens]
        clause_segments_catted = torch.cat(clause_segments)
        nonzero_indices, nonzero_lens, segments = prepare_segments(
            clause_segments_catted
        )
        # Collapse the conjecture clauses

        clauses = segment_reduce(clauses, segments.to(self.device), self.device)

        conjecture_indices_after_collapse = exclusive_cumulative_sum(
            torch.tensor(clause_segment_lens)
        )
        # assert 2 > 3
        all_zeros = torch.zeros(clauses.shape[0])

        if not len(problem_lens[2]) == 1:
            # If there is only 1 "conjecture" being forwarded through the graph, we don't need to do this, and all can remain 0
            # all_zeros.index_copy_(0, torch.tensor(problem_lens[2]), torch.ones(len(problem_lens[2]), dtype=torch.float))
            # all_zeros.index_copy_(0, torch.tensor(conjecture_indices[1:]), torch.ones(len(conjecture_indices[1:]), dtype=torch.float))
            all_zeros.index_copy_(
                0,
                torch.tensor(conjecture_indices_after_collapse[1:]),
                torch.ones(
                    len(conjecture_indices_after_collapse[1:]), dtype=torch.float
                ),
            )
        # print(all_zeros)
        # assert 2 > 3
        # Get a list of [0, 0, 0, 0, 1 , 1, 1 ...] that indicates which problem (and thus, which conjecture) the clause
        # Came with

        ###filltensor = torch.tensor(torch.cumsum(all_zeros, 0), dtype=torch.long)
        filltensor = torch.cumsum(all_zeros, 0).long()
        
        # print(filltensor)
        # assert 2 > 3
        # Creating a mask
        all_zeros1 = torch.zeros(clauses.shape[0])

        all_zeros1.scatter_(0, conjecture_indices_after_collapse, 1)
        conjecture_locations = all_zeros1.bool()
        # print(conjecture_locations)
        # assert 2 > 3
        # Create masked tensors
        premises = clauses[~conjecture_locations]
        conjectures = clauses[conjecture_locations]

        # Concat the conjecture representations to each clause
        selected_conjectures = conjectures[
            filltensor[~conjecture_locations]
        ]  # We duplicate the conjecture vectors so we can concat


        clause_representations = torch.cat((selected_conjectures, premises), dim=1)

        # Get logits
        clause_logits = self.clause_decider(clause_representations)
        # print(clause_logits)
        # print(clause_logits.shape)
        # print(clause_segment_lens)
        num_prems_without_conj = [k-1 for k in clause_segment_lens]
        split_logits = torch.split(clause_logits, num_prems_without_conj)
        # print([k.shape for k in split_logits])
        # assert 2 > 3
        return [k.cpu().detach().numpy() for k in split_logits]

    def train(self, batch, optimizer, loss, balanced_loss=True):

        graph_info, output_info = batch

        # Structure of this data:
        # Labels[0] contain length of segments for the premises and conjecture! These CAN be more than
        # one "clause" long so you have to collapse them
        # Labels[1] contains the label of each premise!

        graph_structure, lens = graph_info

        labels = [torch.tensor(k[1], dtype=torch.float) for k in output_info]
        clause_segments = [torch.tensor(k[0], dtype=torch.long) for k in output_info]

        labels = torch.cat(labels)

        optimizer.zero_grad()
        y_hat = self.forward(graph_structure, lens, clause_segments)

        labels_bool = torch.as_tensor(labels, dtype=torch.bool)
        pos_mask = labels_bool == True
        neg_mask = labels_bool == False
        if not balanced_loss:

            loss = loss_function(y_hat.reshape(-1), labels.to(self.device))
        else:

            num_pos = sum(pos_mask).item()
            num_neg = sum(neg_mask).item()
            # print(num_pos, num_neg)
            # print(pos_mask)
            # print(neg_mask)
            # print(y_hat.shape)
            # print(labels.shape)
            loss = loss_function(y_hat.reshape(-1), labels.to(self.device).reshape(-1))
            # print(loss.shape, pos_mask.shape, neg_mask.shape)
            if not num_pos == 0:
                loss_on_pos = loss[pos_mask].mean()
            else:
                loss_on_pos = 0

            if not num_neg == 0:
                loss_on_neg = loss[neg_mask].mean()
            else:
                loss_on_neg = 0

            # print(loss_on_pos, loss_on_neg)
            loss = (loss_on_pos + loss_on_neg) / 2.0

        tpr_mask = y_hat.reshape(-1)[pos_mask] > 0
        # print(tpr_mask
        if not torch.all(~tpr_mask):
            tpr = torch.as_tensor(tpr_mask, dtype=torch.float).mean().item()
        else:
            tpr = 0.0
        # print(tpr)
        tnr_mask = y_hat.reshape(-1)[neg_mask] < 0
        if not torch.all(~tnr_mask):
            # if not all predictions are positive
            tnr = torch.as_tensor(tnr_mask, dtype=torch.float).mean().item()
        else:
            # if all preds were positive, there are no true negatives that were caught
            tnr = 0.0

        loss.backward()
        optimizer.step()

        return (loss.item(), tpr, tnr)

    def evaluation(self, batch, loss):
        """"This function is supposed to give back the tpr, tnr, the mean of those and the loss on a validation set (5%)
        Without doing any backprop or parameter changes
        """

        balanced_loss = True

        graph_info, output_info = batch

        # Structure of this data:
        # Labels[0] contain length of segments for the premises and conjecture! These CAN be more than
        # one "clause" long so you have to collapse them
        # Labels[1] contains the label of each premise!

        graph_structure, lens = graph_info

        labels = [torch.tensor(k[1], dtype=torch.float) for k in output_info]
        clause_segments = [torch.tensor(k[0], dtype=torch.long) for k in output_info]

        labels = torch.cat(labels)
        # clause_segments = torch.cat(clause_segments)
        y_hat = self.forward(graph_structure, lens, clause_segments)
        labels_bool = torch.as_tensor(labels, dtype=torch.bool)
        pos_mask = labels_bool == True
        neg_mask = labels_bool == False

        if not balanced_loss:

            loss = loss_function(y_hat.reshape(-1), labels.to(self.device))
        else:

            num_pos = sum(pos_mask).item()
            num_neg = sum(neg_mask).item()

            loss = loss_function(y_hat.reshape(-1), labels.to(self.device).reshape(-1))
            if not num_pos == 0:
                loss_on_pos = loss[pos_mask].mean()
            else:
                loss_on_pos = 0

            if not num_neg == 0:
                loss_on_neg = loss[neg_mask].mean()
            else:
                loss_on_neg = 0

            loss = (loss_on_pos + loss_on_neg) / 2.0

            # Now calculate the tpr and tnr
            # How many preds for the pos were > 0?
        tpr_mask = y_hat.reshape(-1)[pos_mask] > 0
        # print(tpr_mask)
        if not torch.all(~tpr_mask):
            tpr = torch.as_tensor(tpr_mask, dtype=torch.float).mean().item()
        else:
            tpr = 0.0
        # print(tpr)
        tnr_mask = y_hat.reshape(-1)[neg_mask] < 0
        if not torch.all(~tnr_mask):
            # if not all predictions are positive
            tnr = torch.as_tensor(tnr_mask, dtype=torch.float).mean().item()
        else:
            # if all preds were positive, there are no true negatives that were caught
            tnr = 0.0
        # print(tnr_mask)
        # print(tnr)

        # print(f"YHAT SHAPE: {y_hat.shape}, {len(y_hat)}")
        return (loss.item(), tpr, tnr), len(y_hat)

    def predict(self, list_of_graphs):
        # print("Got the message")
        graph_info_batch, output_info = batch_graphs(list_of_graphs) # modified this to behave well when the labels are not there
        # print("Batched the list of graphs")
        # (big_graph, segments) label_infos
        # graph_info, output_info = graph_info_batch
        # print(graph_info)
        # print("len")
        # print(type(graph_info))
        # print(len(graph_info))
        # print(output_info)
        # print("Decomposed the batch")
        # print("LEN: ", len(graph_info))
        # print(graph_info[0])
        # print(graph_info[1])
        graph_structure, lens = graph_info_batch
        # print(graph_structure)
        # print(lens)
        # print("Decomposed Graph Info")
        # assert 2 > 3
        fof_to_c = [k[0] for k in output_info]
        # fof_to_c = [torch.tensor(k[1], dtype=torch.long) for k in lens]
        # print(clause_segments)
        # print(problem_lens)
        # print(fof_to_c)
        # assert 2 > 3
        # print("Calculated clause segments")
        y_hat = self.forward(graph_structure, lens, fof_to_c)
        # y_hat = [np.random.randn(10)]
        # print(y_hat)
        torch.cuda.empty_cache()
        return y_hat
        # return y_hat.cpu().detach().numpy()


if __name__ == "__main__":

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

    expname = f"{EXPNAME}"

    no_layers = 10
    no_layers = LAYERS
    model = PIEGNN(
        (16, 16, 16),
        (16, 16, 16),
        hidden_dim=16,
        layers=no_layers,
        residual=True,
        normalization="layer",
        pool_proc_id=0
    )
    model = model.to(model.device)

    torch.save(model.state_dict(), "pt_server_test_model.pt")
    # optimizer = torch.optim.Adam(model.parameters(), lr=LEARNING_RATE)
    loss_function = nn.BCEWithLogitsLoss(reduction="none")
    #
    total_data = load_data(
        "/home/jelle/projects/piegnn_server_integration/problems_small_np", sample=20
    )  # -1 means take all
    batch_size = 4
    j = 0
    vb = total_data[j: j + batch_size]

    float_predictions = model.predict(
        vb
    )

    print(float_predictions)
