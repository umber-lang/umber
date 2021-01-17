(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/AsPattern.expected tokens/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/AsPattern.expected ast/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Basics.expected tokens/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Basics.expected ast/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Classics.expected tokens/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Classics.expected ast/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/CrossModuleUsage.expected tokens/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/CrossModuleUsage.expected ast/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Empty.expected tokens/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Empty.expected ast/Empty.out))))

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
   (diff tokens/ErrorDuplicateLet.expected tokens/ErrorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorDuplicateLet.expected ast/ErrorDuplicateLet.out))))

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
   (diff tokens/ErrorDuplicateVal.expected tokens/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorDuplicateVal.expected ast/ErrorDuplicateVal.out))))

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
   (diff tokens/ErrorPolymorphicRecursion.expected
    tokens/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorPolymorphicRecursion.expected
    ast/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/ExternInt.expected tokens/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/ExternInt.expected ast/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Functions.expected tokens/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Functions.expected ast/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Imports.expected tokens/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Imports.expected ast/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/LetBindingGroups.expected tokens/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/LetBindingGroups.expected ast/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/LetPattern.expected tokens/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/LetPattern.expected ast/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Modules.expected tokens/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Modules.expected ast/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/MutualRecursion.expected tokens/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/MutualRecursion.expected ast/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Operators.expected tokens/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Operators.expected ast/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Option.expected tokens/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Option.expected ast/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Text.expected tokens/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Text.expected ast/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Traits.expected tokens/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Traits.expected ast/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/TypeChecking.expected tokens/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/TypeChecking.expected ast/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Types.expected tokens/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Types.expected ast/Types.out))))

