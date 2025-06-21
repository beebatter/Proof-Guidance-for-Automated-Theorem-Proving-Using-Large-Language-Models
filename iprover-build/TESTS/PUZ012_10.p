%------------------------------------------------------------------------------
% File     : PUZ012_10 : TPTP v8.2.0. Released v8.2.0.
% Domain   : Puzzles
% Problem  : The Mislabeled Boxes
% Version  : PUZ012_1 with the conjecture removed
% English  : There are three boxes a, b, and c on a table. Each box contains
%            apples or bananas or oranges. No two boxes contain the same
%            thing. Each box has a label that says it contains apples or says
%            it contains bananas or says it contains oranges. No box contains
%            what it says on its label. The label on box a says "apples".
%            The label on box b says "oranges". The label on box c says
%            "bananas". You pick up box b and it contains apples. What do
%            the other two boxes contain?

% Refs     : [WO+92] Wos et al. (1992), Automated Reasoning: Introduction a
%          : [Wos88] Wos (1988), Automated Reasoning - 33 Basic Research Pr
% Source   : [TPTP]
% Names    : 

% Status   : Satisfiable
% Rating   : 0.00 v8.2.0
% Syntax   : Number of formulae    :   29 (  12 unt;  12 typ;   0 def)
%            Number of atoms       :   26 (   0 equ)
%            Maximal formula atoms :    3 (   1 avg)
%            Number of connectives :   16 (   7   ~;   4   |;   3   &)
%                                         (   0 <=>;   2  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    6 (   3 avg)
%            Maximal term depth    :    1 (   1 avg)
%            Number of types       :    3 (   2 usr)
%            Number of type conns  :    8 (   4   >;   4   *;   0   +;   0  <<)
%            Number of predicates  :    4 (   4 usr;   0 prp; 2-2 aty)
%            Number of functors    :    6 (   6 usr;   6 con; 0-0 aty)
%            Number of variables   :   12 (  12   !;   0   ?;  12   :)
% SPC      : TF0_SAT_NEQ_NAR

% Comments :
%------------------------------------------------------------------------------
tff(box_type,type,
    box: $tType ).

tff(fruit_type,type,
    fruit: $tType ).

tff(boxa_type,type,
    boxa: box ).

tff(boxb_type,type,
    boxb: box ).

tff(boxc_type,type,
    boxc: box ).

tff(apples_type,type,
    apples: fruit ).

tff(bananas_type,type,
    bananas: fruit ).

tff(oranges_type,type,
    oranges: fruit ).

tff(equal_fruits_type,type,
    equal_fruits: ( fruit * fruit ) > $o ).

tff(equal_boxes_type,type,
    equal_boxes: ( box * box ) > $o ).

tff(contains_type,type,
    contains: ( box * fruit ) > $o ).

tff(label_type,type,
    label: ( box * fruit ) > $o ).

tff(reflexivity_for_fruits,axiom,
    ! [X: fruit] : equal_fruits(X,X) ).

tff(reflexivity_for_boxes,axiom,
    ! [X: box] : equal_boxes(X,X) ).

tff(label_is_wrong,axiom,
    ! [X: box,Y: fruit] :
      ~ ( label(X,Y)
        & contains(X,Y) ) ).

tff(each_thing_is_in_a_box,axiom,
    ! [X: fruit] :
      ( contains(boxa,X)
      | contains(boxb,X)
      | contains(boxc,X) ) ).

tff(each_box_contains_something,axiom,
    ! [X: box] :
      ( contains(X,apples)
      | contains(X,bananas)
      | contains(X,oranges) ) ).

tff(contains_is_well_defined1,axiom,
    ! [X: box,Y: fruit,Z: fruit] :
      ( ( contains(X,Y)
        & contains(X,Z) )
     => equal_fruits(Y,Z) ) ).

tff(contains_is_well_defined2,axiom,
    ! [X: box,Y: fruit,Z: box] :
      ( ( contains(X,Y)
        & contains(Z,Y) )
     => equal_boxes(X,Z) ) ).

tff(boxa_not_boxb,axiom,
    ~ equal_boxes(boxa,boxb) ).

tff(boxb_not_boxc,axiom,
    ~ equal_boxes(boxb,boxc) ).

tff(boxa_not_boxc,axiom,
    ~ equal_boxes(boxa,boxc) ).

tff(apples_not_bananas,axiom,
    ~ equal_fruits(apples,bananas) ).

tff(bananas_not_oranges,axiom,
    ~ equal_fruits(bananas,oranges) ).

tff(apples_not_oranges,axiom,
    ~ equal_fruits(apples,oranges) ).

tff(boxa_labelled_apples,hypothesis,
    label(boxa,apples) ).

tff(boxb_labelled_oranges,hypothesis,
    label(boxb,oranges) ).

tff(boxc_labelled_bananas,hypothesis,
    label(boxc,bananas) ).

tff(boxb_contains_apples,hypothesis,
    contains(boxb,apples) ).

% tff(prove_boxa_contains_bananas_and_boxc_oranges,conjecture,
%     ( contains(boxa,bananas)
%     & contains(boxc,oranges) ) ).

%------------------------------------------------------------------------------
