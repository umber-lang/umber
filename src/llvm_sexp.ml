open! Import

type llvalue = Llvm.llvalue

let sexp_of_llvalue value = Sexp.Atom (Llvm.string_of_llvalue value)

type llmodule = Llvm.llmodule

let sexp_of_llmodule module_ = Sexp.Atom (Llvm.string_of_llmodule module_)

module Opcode = struct
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

module Value_kind = struct
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