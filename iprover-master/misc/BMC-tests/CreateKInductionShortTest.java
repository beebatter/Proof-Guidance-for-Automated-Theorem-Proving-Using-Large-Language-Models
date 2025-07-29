import java.io.*;
import java.util.*;

class CreateKInductionShortTest {

	static String currVar = "VarCurr";
	static String nextVar = "VarNext";
	static String nextPred = "~$$nextState(VarCurr,VarNext)";
	static String b0 = "$$constB0";
	static String sk = "sK_VarCurr";
	static String x = "x";
	static String xCur = x + "(VarCurr)";
	static String negX = neg(xCur);

	static String neg(String str) {
		return "~" + str;
	}

	// problem type
	public enum ProblemType {
		POS("pos"), NEG("neg"), SAT("sat");
		private ProblemType(String s) {
			name = s;
		}

		final String name;

		public String getName() {
			return name;
		}
	}

	final String trans; // (~next) or (x | next)

	final ProblemType problemType;

	void printProblemType() {
		switch (problemType) {
		case POS:
			out.println("% This problem can be PROVED using k-indunction at " + size + " bounds");
			out.println("% It might be also PROVED using property lemmas");
			break;
		case NEG:
			out.println("% This problem can be PROVED using k-indunction with --use-non-equal true at " + (2 * size)
					+ " bounds");
			out.println("% This problem can not be PROVED using simple k-induction");
			break;
		case SAT:
			out.println("% This problem can be DISPROVED using BMC at " + size + " bounds");
			break;
		}
	}

	void printHeader() {
		out.println("%----------------------------------------------");
		out.println("% problem: " + getProblemName());
		out.println("% category: bit-vector loop problem");
		out.println("%\n% notes:");
		printProblemType();
		out.println("%----------------------------------------------\n");
		out.println("tff(sort_def_8,type, $$state_type: $tType ).");
		out.println("tff(sort_def_9,type, $$address_type: $tType | $attr($addressMaxWidth, 0) ).");
		out.println("tff(sort_def_10,type, $$bitindex_type: $tType ).");
		out.println("tff(func_def_0,type, $$constB0: $$state_type | $attr($state_constant, 0) ).");
		out.println("tff(func_def_1,type, " + sk + ": $$state_type ).");
		out.println("tff(pred_def_12,type, $$nextState: ($$state_type * $$state_type) > $o | $attr($built_in, vapi_pre_processing) ).");
		out.println("tff(pred_def_x,type, " + x + ": $$state_type > $o | $attr($bit_vector, 1) ).");
	}

	abstract class ClauseBase {
		String name;
		String type;
		List<String> atoms = new ArrayList<String>();

		ClauseBase(String type, String name, String... strings) {
			this.name = name;
			this.type = type;
			for (String s : strings) {
				atoms.add(s);
			}
		}

		ClauseBase(String type, String name, List<String> strings) {
			this.name = name;
			this.type = type;
			for (String s : strings) {
				atoms.add(s);
			}
		}

		void print() {
			if (atoms.size() == 0) {
				out.println("% " + name);
			} else {
				out.print("cnf(" + name + "," + type + ",\n\t" + atoms.get(0));
				for (int i = 1; i < atoms.size(); i++)
					out.print(" | " + atoms.get(i));
				out.println(").");
			}
		}
	}

	class Clause extends ClauseBase {
		Clause(String name, String... strings) {
			super("axiom", name, strings);
		}
	}

	class NegConj extends ClauseBase {
		NegConj(String name, String... strings) {
			super("negated_conjecture", name, strings);
		}

		NegConj(String name, List<String> strings) {
			super("negated_conjecture", name, strings);
		}
	}

	List<ClauseBase> clauses = new ArrayList<CreateKInductionShortTest.ClauseBase>();

	void addClause(ClauseBase cl) {
		clauses.add(cl);
	}

	abstract class BitVec_bit {
		final String name;
		final int size;

		BitVec_bit(String n, int sz) {
			name = n;
			size = sz;
		}

		public String getName() {
			return name;
		}

		public int size() {
			return this.size;
		}

		String getBitSafe(int bit) {
			if (bit >= 0 && bit < size) {
				return getBit(bit);
			} else
				return name + "_unknown";
		}

		private String getBit(int bit) {
			return name + "_" + bit;
		}

		private String getPara(String var) {
			return "(" + var + ")";
		}

		// get predicate wrt bit and a parameter (state var)
		public String getPredicate(int bit, String var) {
			return getBit(bit) + getPara(var);
		}

		private void printBitDef(int bit) {
			out.println("tff(pred_def_" + name + "_bit_" + bit + ",type, " + getBit(bit)
					+ ": $$state_type > $o | $attr($bit_vector, 1) ).");
		}

		public void printDef() {
			out.println("% definitions for bits of " + name);
			for (int i = 0; i < size; i++) {
				printBitDef(i);
			}
		}
	}

	class ShiftBitVec_bit extends BitVec_bit {
		final boolean cyclic;

		ShiftBitVec_bit(String n, int sz, boolean c) {
			super(n, sz);
			cyclic = c;
		}

		// copy index i into index j: {~bit_i,bit_j} and {bit_i,~bit_j}
		void copy(int i, int j) {
			String cur = getPredicate(i, currVar);
			String next = getPredicate(j, nextVar);
			addClause(new Clause("Copy bit " + i + " into " + j + " for vector " + name));
			addClause(new Clause(name + "_set_" + j + "_after_" + i, neg(cur), next, trans));
			addClause(new Clause(name + "_drop_" + j + "_after_" + i, cur, neg(next), trans));
		}

		// zero index i: {~bit_i(next)}
		void zero(int i) {
			addClause(new Clause("Zero bit " + i + " for vector " + name));
			String next = getPredicate(i, nextVar);
			addClause(new Clause(name + "_drop_" + i, neg(next), trans));
		}

		void getClauses() {
			if (!cyclic)
				zero(0);
			for (int i = 0; i < size - 1; i++) {
				copy(i, i + 1);
			}
			if (cyclic)
				copy(size - 1, 0);
		}
	}

	class Accumulator_bit extends BitVec_bit {
		final BitVec_bit input;

		Accumulator_bit(String name, BitVec_bit input) {
			super(name, input.size());
			this.input = input;
		}

		private void printCopy(int bit) {
			addClause(new Clause(""));
			addClause(new Clause("Add bit " + bit + " to accumulator " + name + " from " + input.getName()));
			String curAcc = getPredicate(bit, currVar);
			String curInp = input.getPredicate(bit, currVar);
			String nextAcc = getPredicate(bit, nextVar);
			// input(cur) -> acc(next)
			addClause(new Clause(input.getName() + "(curr) -> " + name + "(next)"));
			addClause(new Clause(name + "_set_" + bit, neg(curInp), nextAcc, trans));
			// acc(cur) -> acc(next)
			addClause(new Clause(name + "(curr) -> " + name + "(next)"));
			addClause(new Clause(name + "_copy1_" + bit, neg(curAcc), nextAcc, trans));
			// ~input(cur), ~acc(cur) -> ~acc(next)
			addClause(new Clause("~" + input.getName() + "(curr), ~" + name + "(curr) -> ~" + name + "(next)"));
			addClause(new Clause(name + "_copy0_" + bit, curInp, curAcc, neg(nextAcc), trans));
		}

		void getClauses() {
			for (int i = 0; i < size; i++) {
				printCopy(i);
			}
		}
	}

	final PrintWriter out;
	final int size;
	final ShiftBitVec_bit b;
	final Accumulator_bit accum;

	String getProblemName() {
		return "bv_loop_k-ind_" + size + "-" + problemType.getName() + ".cnf";
	}

	public CreateKInductionShortTest(int n, ProblemType pt) throws FileNotFoundException, UnsupportedEncodingException {
		this.size = n;
		problemType = pt;
		System.out.println("CreateKInductionShortTest for " + n + " and " + problemType.getName());
		out = new PrintWriter(getProblemName(), "UTF-8");
		if (problemType == ProblemType.NEG)
			trans = xCur + " | " + nextPred;
		else
			trans = nextPred;
		b = new ShiftBitVec_bit("b", n, false);
		accum = new Accumulator_bit("acc", b);
		b.getClauses();
		accum.getClauses();
		if (problemType == ProblemType.NEG)
			build_x();

		build_init();
		build_target();
	}

	private void build_x() {
		addClause(new Clause(""));
		addClause(new Clause("Keep value of x"));
		addClause(new Clause("keep_x_0", xCur, neg(x + "(VarNext)"), nextPred));
		addClause(new Clause("keep_x_1", neg(xCur), x + "(VarNext)", nextPred));
		addClause(new Clause(""));
		addClause(new Clause("Non-det transitions: if(X) then (acc'=0) OR (b'=1)"));
		addClause(new Clause(""));
		addClause(new Clause("Corresponding clause is: {~X, (acc_n'=0), b'=1, ~next}"));
		addClause(new Clause(""));
		String b1 = b.getPredicate(0, nextVar);
		String nx = neg(xCur); // ~X
		String acn = neg(accum.getPredicate(accum.size() - 1, nextVar));
		String[] accums = { acn };
		int n = 0;
		// cycle through accum bits
		for (String ac : accums) {
			addClause(new Clause("trans_x_" + (n++), nx, ac, b1, nextPred));
		}
	}

	private void build_init() {
		addClause(new Clause(""));
		addClause(new Clause("Init clauses:"));
		addClause(new Clause(""));
		addClause(new Clause("Init " + b.getName() + " with 0-th bit on"));
		// it's enough to have b0 at 1, the rest doesn't matter. TODO: try set
		// b1...bn at 0 and check the difference
		addClause(new Clause("init_" + b.getName() + "_set_0", b.getPredicate(0, b0)));
		for (int i = 1; i < b.size(); i++)
			addClause(new Clause("init_" + b.getName() + "_drop_" + i, neg(b.getPredicate(i, b0))));
		addClause(new Clause(""));
		addClause(new Clause(""));
		addClause(new Clause("Init " + accum.getName() + " with 0s everywhere"));
		for (int i = 0; i < accum.size(); i++) {
			addClause(new Clause("init_" + accum.getName() + "_drop_" + i, neg(accum.getPredicate(i, b0))));
		}
		if (problemType == ProblemType.NEG) {
			addClause(new Clause(""));
			addClause(new Clause("Init X with 0"));
			addClause(new Clause("init_x_with_0", neg(x + "(" + b0 + ")")));
		}
	}

	private void build_target() {
		addClause(new Clause(""));
		if (problemType == ProblemType.SAT)
			addClause(new Clause("Target clauses: any of basic is true"));
		else
			addClause(new Clause("Target clauses: any of basic is true OR all accum bits are ones"));
		addClause(new Clause(""));

		for (int i = 0; i < b.size(); i++)
			addClause(new NegConj("target_" + b.getName() + "_bit_" + i, neg(b.getPredicate(i, sk))));
		List<String> atoms = new ArrayList<String>();
		for (int i = 0; i < accum.size(); i++)
			atoms.add(neg(accum.getPredicate(i, sk)));
		if (problemType != ProblemType.SAT)
			addClause(new NegConj("target_" + accum.getName(), atoms));
	}

	public void print() {
		printHeader();
		b.printDef();
		accum.printDef();
		for (ClauseBase cl : clauses) {
			cl.print();
		}
		out.close();
	}

	public static void main(String[] args) {
		int max = 10;
		if (args.length > 0)
			max = Integer.parseInt(args[0]);
		if (max < 3)
			max = 3;
		CreateKInductionShortTest p;
		try {
			// for (int i = 2; i < max; i++)
			// for (boolean b : bs) {
			// p = new CreateKInductionShortTest(i, b);
			// p.print(i);
			// }
			for (int n = 2; n < 8200; n *= 2) {
				for (ProblemType pt : ProblemType.values()) {
					p = new CreateKInductionShortTest(n, pt);
					p.print();
				}
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
	}
}