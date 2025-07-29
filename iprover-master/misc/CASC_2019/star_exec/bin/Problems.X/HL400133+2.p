include('Axioms/HL4001+2.ax').
fof(mem_c_2Emin_2E_3D,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => mem(c_2Emin_2E_3D(A_27a),arr(A_27a,arr(A_27a,bool))) ) )).

fof(ax_eq_p,axiom,(
    ! [A] :
      ( ne(A)
     => ! [X] :
          ( mem(X,A)
         => ! [Y] :
              ( mem(Y,A)
             => ( p(ap(ap(c_2Emin_2E_3D(A),X),Y))
              <=> X = Y ) ) ) ) )).

fof(mem_c_2Emin_2E_40,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => mem(c_2Emin_2E_40(A_27a),arr(arr(A_27a,bool),A_27a)) ) )).

fof(mem_c_2Ebool_2E_21,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => mem(c_2Ebool_2E_21(A_27a),arr(arr(A_27a,bool),bool)) ) )).

fof(ax_all_p,axiom,(
    ! [A] :
      ( ne(A)
     => ! [Q] :
          ( mem(Q,arr(A,bool))
         => ( p(ap(c_2Ebool_2E_21(A),Q))
          <=> ! [X] :
                ( mem(X,A)
               => p(ap(Q,X)) ) ) ) ) )).

fof(mem_c_2Ebool_2E_2F_5C,axiom,(
    mem(c_2Ebool_2E_2F_5C,arr(bool,arr(bool,bool))) )).

fof(ax_and_p,axiom,(
    ! [Q] :
      ( mem(Q,bool)
     => ! [R] :
          ( mem(R,bool)
         => ( p(ap(ap(c_2Ebool_2E_2F_5C,Q),R))
          <=> ( p(Q)
              & p(R) ) ) ) ) )).

fof(mem_c_2Ebool_2EIN,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => mem(c_2Ebool_2EIN(A_27a),arr(A_27a,arr(arr(A_27a,bool),bool))) ) )).

fof(mem_c_2Ebool_2ERES__SELECT,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => mem(c_2Ebool_2ERES__SELECT(A_27a),arr(arr(A_27a,bool),arr(arr(A_27a,bool),A_27a))) ) )).

fof(lamtp_f39,axiom,(
    ! [A_27a,V0p] :
      ( mem(V0p,arr(A_27a,bool))
     => ! [V1m] :
          ( mem(V1m,arr(A_27a,bool))
         => mem(f39(A_27a,V0p,V1m),arr(A_27a,bool)) ) ) )).

fof(lameq_f39,axiom,(
    ! [A_27a,V0p] :
      ( mem(V0p,arr(A_27a,bool))
     => ! [V1m] :
          ( mem(V1m,arr(A_27a,bool))
         => ! [V2x] :
              ( mem(V2x,A_27a)
             => ap(f39(A_27a,V0p,V1m),V2x) = ap(ap(c_2Ebool_2E_2F_5C,ap(ap(c_2Ebool_2EIN(A_27a),V2x),V0p)),ap(V1m,V2x)) ) ) ) )).

fof(lamtp_f46,axiom,(
    ! [A_27a,A_27a,V0p] :
      ( mem(V0p,arr(A_27a,bool))
     => mem(f46(A_27a,A_27a,V0p),arr(arr(A_27a,bool),A_27a)) ) )).

fof(lameq_f46,axiom,(
    ! [A_27a,A_27a,V0p] :
      ( mem(V0p,arr(A_27a,bool))
     => ! [V1m] :
          ( mem(V1m,arr(A_27a,bool))
         => ap(f46(A_27a,A_27a,V0p),V1m) = ap(c_2Emin_2E_40(A_27a),f39(A_27a,V0p,V1m)) ) ) )).

fof(lamtp_f45,axiom,(
    ! [A_27a,A_27a,A_27a] : mem(f45(A_27a,A_27a,A_27a),arr(arr(A_27a,bool),arr(arr(A_27a,bool),A_27a))) )).

fof(lameq_f45,axiom,(
    ! [A_27a,A_27a,A_27a,V0p] :
      ( mem(V0p,arr(A_27a,bool))
     => ap(f45(A_27a,A_27a,A_27a),V0p) = f46(A_27a,A_27a,V0p) ) )).

fof(ax_thm_2Ebool_2ERES__SELECT__DEF,axiom,(
    ! [A_27a] :
      ( ne(A_27a)
     => c_2Ebool_2ERES__SELECT(A_27a) = f45(A_27a,A_27a,A_27a) ) )).

fof(lamtp_f64,axiom,(
    ! [A_27a,V0P] :
      ( mem(V0P,arr(A_27a,bool))
     => ! [V1f] :
          ( mem(V1f,arr(A_27a,bool))
         => mem(f64(A_27a,V0P,V1f),arr(A_27a,bool)) ) ) )).

fof(lameq_f64,axiom,(
    ! [A_27a,V0P] :
      ( mem(V0P,arr(A_27a,bool))
     => ! [V1f] :
          ( mem(V1f,arr(A_27a,bool))
         => ! [V2x] :
              ( mem(V2x,A_27a)
             => ap(f64(A_27a,V0P,V1f),V2x) = ap(ap(c_2Ebool_2E_2F_5C,ap(ap(c_2Ebool_2EIN(A_27a),V2x),V0P)),ap(V1f,V2x)) ) ) ) )).

fof(conj_thm_2Ebool_2ERES__SELECT__THM,conjecture,(
    ! [A_27a] :
      ( ne(A_27a)
     => ! [V0P] :
          ( mem(V0P,arr(A_27a,bool))
         => ! [V1f] :
              ( mem(V1f,arr(A_27a,bool))
             => ap(ap(c_2Ebool_2ERES__SELECT(A_27a),V0P),V1f) = ap(c_2Emin_2E_40(A_27a),f64(A_27a,V0P,V1f)) ) ) ) )).
