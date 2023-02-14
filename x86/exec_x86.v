Require Import Coqlib Maps.
Require Import AST Integers Values Memory Globalenvs Asm.

Definition exec_i (regs : list Z) i :=
  let sig := {|
    sig_args := nil;
    sig_res := Tvoid;
    sig_cc := cc_default
  |} in
  let f := {| fn_sig := sig; fn_code := i :: nil |} in
  let fd := Internal f in
  let prog : program := {|
    prog_defs := (1%positive, Gfun fd) :: nil;
    prog_public := 1%positive :: nil;
    prog_main := 1%positive
  |} in
  let regset r :=
    match r with
    | PC => Vint (Int.repr 1)
    | IR r =>
      (* TODO: check the order to be consistent with Jasmin *)
      Vlong (Int64.repr (nth
        match r with
        | RAX => 0
        | RBX => 1
        | RCX => 2
        | RDX => 3
        | RSI => 4
        | RDI => 5
        | RBP => 6
        | RSP => 7
        | R8 => 8
        | R9 => 9
        | R10 => 10
        | R11 => 11
        | R12 => 12
        | R13 => 13
        | R14 => 14
        | R15 => 15
        end regs 0))
    | _ => Vundef
    end
  in
  exec_instr (Genv.globalenv prog) f i regset Mem.empty.
