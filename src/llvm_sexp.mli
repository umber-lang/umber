open! Import

type llvalue = Llvm.llvalue [@@deriving sexp_of]
type llmodule = Llvm.llmodule [@@deriving sexp_of]

module Opcode : sig
  type t = Llvm.Opcode.t =
    | Invalid (** Not an instruction *)
    | Ret (** Terminator Instructions *)
    | Br
    | Switch
    | IndirectBr
    | Invoke
    | Invalid2
    | Unreachable
    | Add (** Standard Binary Operators *)
    | FAdd
    | Sub
    | FSub
    | Mul
    | FMul
    | UDiv
    | SDiv
    | FDiv
    | URem
    | SRem
    | FRem
    | Shl (** Logical Operators *)
    | LShr
    | AShr
    | And
    | Or
    | Xor
    | Alloca (** Memory Operators *)
    | Load
    | Store
    | GetElementPtr
    | Trunc (** Cast Operators *)
    | ZExt
    | SExt
    | FPToUI
    | FPToSI
    | UIToFP
    | SIToFP
    | FPTrunc
    | FPExt
    | PtrToInt
    | IntToPtr
    | BitCast
    | ICmp (** Other Operators *)
    | FCmp
    | PHI
    | Call
    | Select
    | UserOp1
    | UserOp2
    | VAArg
    | ExtractElement
    | InsertElement
    | ShuffleVector
    | ExtractValue
    | InsertValue
    | Fence
    | AtomicCmpXchg
    | AtomicRMW
    | Resume
    | LandingPad
    | AddrSpaceCast
    | CleanupRet
    | CatchRet
    | CatchPad
    | CleanupPad
    | CatchSwitch
    | FNeg
    | CallBr
  [@@deriving sexp]
end

module Value_kind : sig
  type t =
    | NullValue
    | Argument
    | BasicBlock
    | InlineAsm
    | MDNode
    | MDString
    | BlockAddress
    | ConstantAggregateZero
    | ConstantArray
    | ConstantDataArray
    | ConstantDataVector
    | ConstantExpr
    | ConstantFP
    | ConstantInt
    | ConstantPointerNull
    | ConstantStruct
    | ConstantVector
    | Function
    | GlobalAlias
    | GlobalIFunc
    | GlobalVariable
    | UndefValue
    | PoisonValue
    | Instruction of Opcode.t
  [@@deriving sexp]
end
