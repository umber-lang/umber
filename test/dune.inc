(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/AsPattern.expected tokens/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/AsPattern.expected ast/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/AsPattern.expected mir/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/AsPattern.expected llvm/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Basics.expected tokens/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Basics.expected ast/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Basics.expected mir/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Basics.expected llvm/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Classics.expected tokens/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Classics.expected ast/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Classics.expected mir/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Classics.expected llvm/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/CrossModuleUsage.expected tokens/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/CrossModuleUsage.expected ast/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/CrossModuleUsage.expected mir/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/CrossModuleUsage.expected llvm/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Empty.expected tokens/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Empty.expected ast/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Empty.expected mir/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Empty.expected llvm/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorCrossModuleMutualRecursion.expected
    tokens/ErrorCrossModuleMutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorCrossModuleMutualRecursion.expected
    ast/ErrorCrossModuleMutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorCrossModuleMutualRecursion.expected
    mir/ErrorCrossModuleMutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorCrossModuleMutualRecursion.expected
    llvm/ErrorCrossModuleMutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorCyclicTypeAlias.expected
    tokens/ErrorCyclicTypeAlias.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorCyclicTypeAlias.expected ast/ErrorCyclicTypeAlias.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorCyclicTypeAlias.expected mir/ErrorCyclicTypeAlias.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorCyclicTypeAlias.expected llvm/ErrorCyclicTypeAlias.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorDuplicateLet.expected tokens/ErrorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorDuplicateLet.expected ast/ErrorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/ErrorDuplicateLet.expected mir/ErrorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorDuplicateLet.expected llvm/ErrorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorDuplicateNameAsPattern.expected
    tokens/ErrorDuplicateNameAsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorDuplicateNameAsPattern.expected
    ast/ErrorDuplicateNameAsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorDuplicateNameAsPattern.expected
    mir/ErrorDuplicateNameAsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorDuplicateNameAsPattern.expected
    llvm/ErrorDuplicateNameAsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorDuplicateVal.expected tokens/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorDuplicateVal.expected ast/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/ErrorDuplicateVal.expected mir/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorDuplicateVal.expected llvm/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorIncompatibleSig.expected
    tokens/ErrorIncompatibleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorIncompatibleSig.expected ast/ErrorIncompatibleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorIncompatibleSig.expected mir/ErrorIncompatibleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorIncompatibleSig.expected llvm/ErrorIncompatibleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorInexhaustivePattern.expected
    tokens/ErrorInexhaustivePattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorInexhaustivePattern.expected
    ast/ErrorInexhaustivePattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorInexhaustivePattern.expected
    mir/ErrorInexhaustivePattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorInexhaustivePattern.expected
    llvm/ErrorInexhaustivePattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorPatternUnionNameTypes.expected
    tokens/ErrorPatternUnionNameTypes.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorPatternUnionNameTypes.expected
    ast/ErrorPatternUnionNameTypes.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorPatternUnionNameTypes.expected
    mir/ErrorPatternUnionNameTypes.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorPatternUnionNameTypes.expected
    llvm/ErrorPatternUnionNameTypes.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorPolymorphicRecursion.expected
    tokens/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorPolymorphicRecursion.expected
    ast/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorPolymorphicRecursion.expected
    mir/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorPolymorphicRecursion.expected
    llvm/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/ExternInt.expected tokens/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/ExternInt.expected ast/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/ExternInt.expected mir/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/ExternInt.expected llvm/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Functions.expected tokens/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Functions.expected ast/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Functions.expected mir/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Functions.expected llvm/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Imports.expected tokens/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Imports.expected ast/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Imports.expected mir/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Imports.expected llvm/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/LetBindingGroups.expected tokens/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/LetBindingGroups.expected ast/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/LetBindingGroups.expected mir/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/LetBindingGroups.expected llvm/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/LetPattern.expected tokens/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/LetPattern.expected ast/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/LetPattern.expected mir/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/LetPattern.expected llvm/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Modules.expected tokens/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Modules.expected ast/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Modules.expected mir/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Modules.expected llvm/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/ModuleSig.expected tokens/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/ModuleSig.expected ast/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/ModuleSig.expected mir/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/ModuleSig.expected llvm/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/MutualRecursion.expected tokens/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/MutualRecursion.expected ast/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/MutualRecursion.expected mir/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/MutualRecursion.expected llvm/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Operators.expected tokens/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Operators.expected ast/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Operators.expected mir/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Operators.expected llvm/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Option.expected tokens/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Option.expected ast/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Option.expected mir/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Option.expected llvm/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Text.expected tokens/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Text.expected ast/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Text.expected mir/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Text.expected llvm/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Traits.expected tokens/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Traits.expected ast/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Traits.expected mir/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Traits.expected llvm/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/TypeChecking.expected tokens/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/TypeChecking.expected ast/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/TypeChecking.expected mir/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/TypeChecking.expected llvm/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Types.expected tokens/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Types.expected ast/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Types.expected mir/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Types.expected llvm/Types.out))))

