#include "cint.h"

void print_circuit()
{
  value *ml_cop_circuit_st = caml_named_value("cop_circuit_st");
  value circuit = caml_callback(*ml_cop_circuit_st, Val_unit);
  printf("Graph:\n");
  value graph = fst(circuit);
  value nums = fst(graph);
  printf("  %ld nodes, %ld funcs, %ld rels\n",
	 Long_val(fst(nums)), Long_val(snd(nums)), Long_val(third(nums)));

  char c;
  value fl;
  for(int b=0; b<2; b++){
    if(b == 0){ c = 'f'; fl = snd(graph); }
    if(b == 1){ c = 'r'; fl = third(graph); }

    for(; fl != Val_emptylist; fl = snd(fl)){
      value res_f_p = fst(fl);
      long int node = Long_val(fst(res_f_p));
      long int fn = Long_val(snd(res_f_p));
      printf("  n%ld <- %c%ld ( ", node, c, fn);
      for(value a = third(res_f_p); a != Val_emptylist; a = snd(a)){
	long int n = Long_val(fst(a));
	printf("n%ld ", n);
      }
      printf(")\n");
    }
  }
  printf("Path:");
  for(value l = snd(circuit); l != Val_emptylist; l = snd(l))
    printf(" n%ld", Long_val(fst(l)));
  printf("\n");
  printf("Goals:");
  for(value l = third(circuit); l != Val_emptylist; l = snd(l))
    printf(" n%ld", Long_val(fst(l)));
  printf("\n");
  printf("Axioms:\n");
  for(value ll = fourth(circuit); ll != Val_emptylist; ll = snd(ll)){
    printf(" ");
    for(value l = fst(ll); l != Val_emptylist; l = snd(l))
      printf(" n%ld", Long_val(fst(l)));
    printf("\n");
  }  
}

void print_partitions(value ld, int in_triples)
{
  value lengths = fst(ld);
  value data = snd(ld);
  int index = 0;

  for(value l = lengths; l != Val_emptylist; l = snd(l)){
    int len = Long_val(fst(l));
    printf("%2d:", index);
    for(int i=0; i<len; i++){
      if(data == Val_emptylist){
	printf("ERROR: run out of data");
	break;
      }
      value x = fst(data);
      if(in_triples)
	printf(" (%ld %ld %ld %ld)", Long_val(fst(x)),
	       Long_val(snd(x)), Long_val(third(x)), Long_val(fourth(x)));
      else printf(" %ld", Long_val(x));
      data = snd(data);
    }
    printf("\n");
    index += 1;
  }
  printf("\n");
}

void print_indices()
{
  value *ml_cop_graph_indices = caml_named_value("cop_graph_indices");
  value nct = caml_callback(*ml_cop_graph_indices, Val_unit);

  for(value l=fst(nct); l != Val_emptylist; l = snd(l))
    print_partitions(fst(l), 1);
  for(value l=snd(nct); l != Val_emptylist; l = snd(l))
    print_partitions(fst(l), 0);
  for(value l=third(nct); l != Val_emptylist; l = snd(l))
    print_caml_list(fst(l));
}

int main()
{
  cop_caml_init();
  value *ml_start = caml_named_value("cop_start");
  value *ml_action = caml_named_value("cop_action");
  //const char *fname = "../b.p";
  const char *fname = "t9_tops_3";
  value sol_an = caml_callback(*ml_start, caml_copy_string(fname));
  printf("%ld %ld\n", Long_val(fst(sol_an)), Long_val(snd(sol_an)));

  /*
  caml_callback(*ml_action, Val_long(0));
  caml_callback(*ml_action, Val_long(1));
  caml_callback(*ml_action, Val_long(1));
  caml_callback(*ml_action, Val_long(1));
  caml_callback(*ml_action, Val_long(3));
  */

  print_circuit();
  //print_indices();
}
