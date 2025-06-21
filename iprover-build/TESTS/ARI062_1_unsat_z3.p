%------------------------------------------------------------------------------
% File     : ARI062_1 : TPTP v8.1.2. Released v5.0.0.
% Domain   : Arithmetic
% Problem  : Integer: Sum 2 and 3 is not 6
% Version  : Especial.
% English  :

% Refs     :
% Source   : [TPTP]
% Names    :

% Status   : Theorem
% Rating   : 0.00 v6.2.0, 0.12 v6.1.0, 0.11 v6.0.0, 0.00 v5.2.0, 0.17 v5.1.0, 0.00 v5.0.0
% Syntax   : Number of formulae    :    1 (   1 unt;   0 typ;   0 def)
%            Number of atoms       :    1 (   1 equ)
%            Maximal formula atoms :    1 (   1 avg)
%            Number of connectives :    1 (   1   ~;   0   |;   0   &)
%                                         (   0 <=>;   0  =>;   0  <=;   0 <~>)
%            Maximal formula depth :    2 (   2 avg)
%            Maximal term depth    :    2 (   1 avg)
%            Number arithmetic     :    4 (   0 atm;   1 fun;   3 num;   0 var)
%            Number of types       :    0 (   0 usr)
%            Number of type conns  :    0 (   0   >;   0   *;   0   +;   0  <<)
%            Number of predicates  :    1 (   0 usr;   0 prp; 2-2 aty)
%            Number of functors    :    4 (   0 usr;   3 con; 0-2 aty)
%            Number of variables   :    0 (   0   !;   0   ?;   0   :)
% SPC      : TF0_THM_EQU_ARI

% Comments :
%------------------------------------------------------------------------------
tff(sum_2_3_not_6,conjecture,
    $sum(2,3) != 6 ).

%------------------------------------------------------------------------------
