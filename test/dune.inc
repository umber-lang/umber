(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/AsPattern.expected tokens/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/AsPattern.expected ast/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/AsPattern.expected mir/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/AsPattern.expected llvm/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/AsPattern.expected output/AsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Basics.expected tokens/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Basics.expected ast/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Basics.expected mir/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Basics.expected llvm/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Basics.expected output/Basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Classics.expected tokens/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Classics.expected ast/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Classics.expected mir/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Classics.expected llvm/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Classics.expected output/Classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Closures.expected tokens/Closures.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Closures.expected ast/Closures.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Closures.expected mir/Closures.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Closures.expected llvm/Closures.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Closures.expected output/Closures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/Constructors.expected tokens/Constructors.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Constructors.expected ast/Constructors.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Constructors.expected mir/Constructors.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Constructors.expected llvm/Constructors.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff output/Constructors.expected output/Constructors.out))))

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
 (action
  (no-infer
   (diff output/CrossModuleUsage.expected output/CrossModuleUsage.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Effects.expected tokens/Effects.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Effects.expected ast/Effects.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Effects.expected mir/Effects.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Effects.expected llvm/Effects.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Effects.expected output/Effects.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Empty.expected tokens/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Empty.expected ast/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Empty.expected mir/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Empty.expected llvm/Empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Empty.expected output/Empty.out))))

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
   (diff output/ErrorCrossModuleMutualRecursion.expected
    output/ErrorCrossModuleMutualRecursion.out))))

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
   (diff output/ErrorCyclicTypeAlias.expected
    output/ErrorCyclicTypeAlias.out))))

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
   (diff output/ErrorDuplicateLet.expected output/ErrorDuplicateLet.out))))

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
   (diff output/ErrorDuplicateNameAsPattern.expected
    output/ErrorDuplicateNameAsPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorDuplicateTypeParameter.expected
    tokens/ErrorDuplicateTypeParameter.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorDuplicateTypeParameter.expected
    ast/ErrorDuplicateTypeParameter.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorDuplicateTypeParameter.expected
    mir/ErrorDuplicateTypeParameter.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorDuplicateTypeParameter.expected
    llvm/ErrorDuplicateTypeParameter.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorDuplicateTypeParameter.expected
    output/ErrorDuplicateTypeParameter.out))))

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
   (diff output/ErrorDuplicateVal.expected output/ErrorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorHangingVal.expected tokens/ErrorHangingVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorHangingVal.expected ast/ErrorHangingVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/ErrorHangingVal.expected mir/ErrorHangingVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/ErrorHangingVal.expected llvm/ErrorHangingVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorHangingVal.expected output/ErrorHangingVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorImportOfImport.expected tokens/ErrorImportOfImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorImportOfImport.expected ast/ErrorImportOfImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorImportOfImport.expected mir/ErrorImportOfImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorImportOfImport.expected llvm/ErrorImportOfImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorImportOfImport.expected output/ErrorImportOfImport.out))))

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
   (diff output/ErrorIncompatibleSig.expected
    output/ErrorIncompatibleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorInconsistentValType.expected
    tokens/ErrorInconsistentValType.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorInconsistentValType.expected
    ast/ErrorInconsistentValType.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorInconsistentValType.expected
    mir/ErrorInconsistentValType.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorInconsistentValType.expected
    llvm/ErrorInconsistentValType.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorInconsistentValType.expected
    output/ErrorInconsistentValType.out))))

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
   (diff output/ErrorInexhaustivePattern.expected
    output/ErrorInexhaustivePattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorLetRecExpression.expected
    tokens/ErrorLetRecExpression.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorLetRecExpression.expected ast/ErrorLetRecExpression.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorLetRecExpression.expected mir/ErrorLetRecExpression.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorLetRecExpression.expected llvm/ErrorLetRecExpression.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorLetRecExpression.expected
    output/ErrorLetRecExpression.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorNameClashExtern.expected
    tokens/ErrorNameClashExtern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorNameClashExtern.expected ast/ErrorNameClashExtern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorNameClashExtern.expected mir/ErrorNameClashExtern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorNameClashExtern.expected llvm/ErrorNameClashExtern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorNameClashExtern.expected
    output/ErrorNameClashExtern.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorNameClashExternVal.expected
    tokens/ErrorNameClashExternVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorNameClashExternVal.expected
    ast/ErrorNameClashExternVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorNameClashExternVal.expected
    mir/ErrorNameClashExternVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorNameClashExternVal.expected
    llvm/ErrorNameClashExternVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorNameClashExternVal.expected
    output/ErrorNameClashExternVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorNameClashWithImport.expected
    tokens/ErrorNameClashWithImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorNameClashWithImport.expected
    ast/ErrorNameClashWithImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorNameClashWithImport.expected
    mir/ErrorNameClashWithImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorNameClashWithImport.expected
    llvm/ErrorNameClashWithImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorNameClashWithImport.expected
    output/ErrorNameClashWithImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorPartialApplication.expected
    tokens/ErrorPartialApplication.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorPartialApplication.expected
    ast/ErrorPartialApplication.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorPartialApplication.expected
    mir/ErrorPartialApplication.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorPartialApplication.expected
    llvm/ErrorPartialApplication.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorPartialApplication.expected
    output/ErrorPartialApplication.out))))

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
   (diff output/ErrorPatternUnionNameTypes.expected
    output/ErrorPatternUnionNameTypes.out))))

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
 (action
  (no-infer
   (diff output/ErrorPolymorphicRecursion.expected
    output/ErrorPolymorphicRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorShadowedBool.expected tokens/ErrorShadowedBool.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/ErrorShadowedBool.expected ast/ErrorShadowedBool.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/ErrorShadowedBool.expected mir/ErrorShadowedBool.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorShadowedBool.expected llvm/ErrorShadowedBool.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorShadowedBool.expected output/ErrorShadowedBool.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorSignatureCompatibility.expected
    tokens/ErrorSignatureCompatibility.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorSignatureCompatibility.expected
    ast/ErrorSignatureCompatibility.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorSignatureCompatibility.expected
    mir/ErrorSignatureCompatibility.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorSignatureCompatibility.expected
    llvm/ErrorSignatureCompatibility.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorSignatureCompatibility.expected
    output/ErrorSignatureCompatibility.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorSubmoduleSignatures.expected
    tokens/ErrorSubmoduleSignatures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorSubmoduleSignatures.expected
    ast/ErrorSubmoduleSignatures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorSubmoduleSignatures.expected
    mir/ErrorSubmoduleSignatures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorSubmoduleSignatures.expected
    llvm/ErrorSubmoduleSignatures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorSubmoduleSignatures.expected
    output/ErrorSubmoduleSignatures.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorToplevelImport.expected tokens/ErrorToplevelImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorToplevelImport.expected ast/ErrorToplevelImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorToplevelImport.expected mir/ErrorToplevelImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorToplevelImport.expected llvm/ErrorToplevelImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorToplevelImport.expected output/ErrorToplevelImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/ErrorUnqualifiedUniversalImport.expected
    tokens/ErrorUnqualifiedUniversalImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/ErrorUnqualifiedUniversalImport.expected
    ast/ErrorUnqualifiedUniversalImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/ErrorUnqualifiedUniversalImport.expected
    mir/ErrorUnqualifiedUniversalImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/ErrorUnqualifiedUniversalImport.expected
    llvm/ErrorUnqualifiedUniversalImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/ErrorUnqualifiedUniversalImport.expected
    output/ErrorUnqualifiedUniversalImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/ExternInt.expected tokens/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/ExternInt.expected ast/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/ExternInt.expected mir/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/ExternInt.expected llvm/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/ExternInt.expected output/ExternInt.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Functions.expected tokens/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Functions.expected ast/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Functions.expected mir/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Functions.expected llvm/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Functions.expected output/Functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/Generalization.expected tokens/Generalization.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/Generalization.expected ast/Generalization.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/Generalization.expected mir/Generalization.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/Generalization.expected llvm/Generalization.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff output/Generalization.expected output/Generalization.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/HelloWorld.expected tokens/HelloWorld.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/HelloWorld.expected ast/HelloWorld.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/HelloWorld.expected mir/HelloWorld.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/HelloWorld.expected llvm/HelloWorld.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/HelloWorld.expected output/HelloWorld.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Imports.expected tokens/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Imports.expected ast/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Imports.expected mir/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Imports.expected llvm/Imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Imports.expected output/Imports.out))))

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
 (action
  (no-infer
   (diff output/LetBindingGroups.expected output/LetBindingGroups.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/LetPattern.expected tokens/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/LetPattern.expected ast/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/LetPattern.expected mir/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/LetPattern.expected llvm/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/LetPattern.expected output/LetPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/ModuleSig.expected tokens/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/ModuleSig.expected ast/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/ModuleSig.expected mir/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/ModuleSig.expected llvm/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/ModuleSig.expected output/ModuleSig.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Modules.expected tokens/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Modules.expected ast/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Modules.expected mir/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Modules.expected llvm/Modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Modules.expected output/Modules.out))))

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
 (action
  (no-infer
   (diff output/MutualRecursion.expected output/MutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/OpSectionLeft.expected tokens/OpSectionLeft.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/OpSectionLeft.expected ast/OpSectionLeft.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/OpSectionLeft.expected mir/OpSectionLeft.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/OpSectionLeft.expected llvm/OpSectionLeft.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff output/OpSectionLeft.expected output/OpSectionLeft.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/OpSectionRight.expected tokens/OpSectionRight.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/OpSectionRight.expected ast/OpSectionRight.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff mir/OpSectionRight.expected mir/OpSectionRight.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/OpSectionRight.expected llvm/OpSectionRight.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff output/OpSectionRight.expected output/OpSectionRight.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Operators.expected tokens/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Operators.expected ast/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Operators.expected mir/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Operators.expected llvm/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Operators.expected output/Operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Option.expected tokens/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Option.expected ast/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Option.expected mir/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Option.expected llvm/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Option.expected output/Option.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/OverlappingImports.expected tokens/OverlappingImports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/OverlappingImports.expected ast/OverlappingImports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/OverlappingImports.expected mir/OverlappingImports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/OverlappingImports.expected llvm/OverlappingImports.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/OverlappingImports.expected output/OverlappingImports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Paths.expected tokens/Paths.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Paths.expected ast/Paths.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Paths.expected mir/Paths.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Paths.expected llvm/Paths.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Paths.expected output/Paths.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/SelfReferentialImport.expected
    tokens/SelfReferentialImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/SelfReferentialImport.expected ast/SelfReferentialImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/SelfReferentialImport.expected mir/SelfReferentialImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/SelfReferentialImport.expected llvm/SelfReferentialImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/SelfReferentialImport.expected
    output/SelfReferentialImport.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Text.expected tokens/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Text.expected ast/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Text.expected mir/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Text.expected llvm/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Text.expected output/Text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Traits.expected tokens/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Traits.expected ast/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Traits.expected mir/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Traits.expected llvm/Traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Traits.expected output/Traits.out))))

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
 (action
  (no-infer (diff output/TypeChecking.expected output/TypeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/TypeModuleNamePunning.expected
    tokens/TypeModuleNamePunning.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff ast/TypeModuleNamePunning.expected ast/TypeModuleNamePunning.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff mir/TypeModuleNamePunning.expected mir/TypeModuleNamePunning.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff llvm/TypeModuleNamePunning.expected llvm/TypeModuleNamePunning.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff output/TypeModuleNamePunning.expected
    output/TypeModuleNamePunning.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/Types.expected tokens/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/Types.expected ast/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/Types.expected mir/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff llvm/Types.expected llvm/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff output/Types.expected output/Types.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/WeirdFunCalls.expected tokens/WeirdFunCalls.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/WeirdFunCalls.expected ast/WeirdFunCalls.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff mir/WeirdFunCalls.expected mir/WeirdFunCalls.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff llvm/WeirdFunCalls.expected llvm/WeirdFunCalls.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff output/WeirdFunCalls.expected output/WeirdFunCalls.out))))

