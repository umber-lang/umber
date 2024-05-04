(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/AsPattern.um})
   (concurrent (diff? tokens/AsPattern.expected tokens/AsPattern.out)
    (diff? ast/AsPattern.expected ast/AsPattern.out)
    (diff? mir/AsPattern.expected mir/AsPattern.out)
    (diff? llvm/AsPattern.expected llvm/AsPattern.out)
    (diff? output/AsPattern.expected output/AsPattern.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Basics.um})
   (concurrent (diff? tokens/Basics.expected tokens/Basics.out)
    (diff? ast/Basics.expected ast/Basics.out)
    (diff? mir/Basics.expected mir/Basics.out)
    (diff? llvm/Basics.expected llvm/Basics.out)
    (diff? output/Basics.expected output/Basics.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Classics.um})
   (concurrent (diff? tokens/Classics.expected tokens/Classics.out)
    (diff? ast/Classics.expected ast/Classics.out)
    (diff? mir/Classics.expected mir/Classics.out)
    (diff? llvm/Classics.expected llvm/Classics.out)
    (diff? output/Classics.expected output/Classics.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Closures.um})
   (concurrent (diff? tokens/Closures.expected tokens/Closures.out)
    (diff? ast/Closures.expected ast/Closures.out)
    (diff? mir/Closures.expected mir/Closures.out)
    (diff? llvm/Closures.expected llvm/Closures.out)
    (diff? output/Closures.expected output/Closures.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Constructors.um})
   (concurrent (diff? tokens/Constructors.expected tokens/Constructors.out)
    (diff? ast/Constructors.expected ast/Constructors.out)
    (diff? mir/Constructors.expected mir/Constructors.out)
    (diff? llvm/Constructors.expected llvm/Constructors.out)
    (diff? output/Constructors.expected output/Constructors.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/CrossModuleUsage.um})
   (concurrent
    (diff? tokens/CrossModuleUsage.expected tokens/CrossModuleUsage.out)
    (diff? ast/CrossModuleUsage.expected ast/CrossModuleUsage.out)
    (diff? mir/CrossModuleUsage.expected mir/CrossModuleUsage.out)
    (diff? llvm/CrossModuleUsage.expected llvm/CrossModuleUsage.out)
    (diff? output/CrossModuleUsage.expected output/CrossModuleUsage.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Effects.um})
   (concurrent (diff? tokens/Effects.expected tokens/Effects.out)
    (diff? ast/Effects.expected ast/Effects.out)
    (diff? mir/Effects.expected mir/Effects.out)
    (diff? llvm/Effects.expected llvm/Effects.out)
    (diff? output/Effects.expected output/Effects.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Empty.um})
   (concurrent (diff? tokens/Empty.expected tokens/Empty.out)
    (diff? ast/Empty.expected ast/Empty.out)
    (diff? mir/Empty.expected mir/Empty.out)
    (diff? llvm/Empty.expected llvm/Empty.out)
    (diff? output/Empty.expected output/Empty.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn
   (run %{dep:test.exe} %{dep:examples/ErrorCrossModuleMutualRecursion.um})
   (concurrent
    (diff? tokens/ErrorCrossModuleMutualRecursion.expected
     tokens/ErrorCrossModuleMutualRecursion.out)
    (diff? ast/ErrorCrossModuleMutualRecursion.expected
     ast/ErrorCrossModuleMutualRecursion.out)
    (diff? mir/ErrorCrossModuleMutualRecursion.expected
     mir/ErrorCrossModuleMutualRecursion.out)
    (diff? llvm/ErrorCrossModuleMutualRecursion.expected
     llvm/ErrorCrossModuleMutualRecursion.out)
    (diff? output/ErrorCrossModuleMutualRecursion.expected
     output/ErrorCrossModuleMutualRecursion.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorCyclicTypeAlias.um})
   (concurrent
    (diff? tokens/ErrorCyclicTypeAlias.expected
     tokens/ErrorCyclicTypeAlias.out)
    (diff? ast/ErrorCyclicTypeAlias.expected ast/ErrorCyclicTypeAlias.out)
    (diff? mir/ErrorCyclicTypeAlias.expected mir/ErrorCyclicTypeAlias.out)
    (diff? llvm/ErrorCyclicTypeAlias.expected llvm/ErrorCyclicTypeAlias.out)
    (diff? output/ErrorCyclicTypeAlias.expected
     output/ErrorCyclicTypeAlias.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn
   (run %{dep:test.exe} %{dep:examples/ErrorDuplicateEffectTypeParameter.um})
   (concurrent
    (diff? tokens/ErrorDuplicateEffectTypeParameter.expected
     tokens/ErrorDuplicateEffectTypeParameter.out)
    (diff? ast/ErrorDuplicateEffectTypeParameter.expected
     ast/ErrorDuplicateEffectTypeParameter.out)
    (diff? mir/ErrorDuplicateEffectTypeParameter.expected
     mir/ErrorDuplicateEffectTypeParameter.out)
    (diff? llvm/ErrorDuplicateEffectTypeParameter.expected
     llvm/ErrorDuplicateEffectTypeParameter.out)
    (diff? output/ErrorDuplicateEffectTypeParameter.expected
     output/ErrorDuplicateEffectTypeParameter.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorDuplicateLet.um})
   (concurrent
    (diff? tokens/ErrorDuplicateLet.expected tokens/ErrorDuplicateLet.out)
    (diff? ast/ErrorDuplicateLet.expected ast/ErrorDuplicateLet.out)
    (diff? mir/ErrorDuplicateLet.expected mir/ErrorDuplicateLet.out)
    (diff? llvm/ErrorDuplicateLet.expected llvm/ErrorDuplicateLet.out)
    (diff? output/ErrorDuplicateLet.expected output/ErrorDuplicateLet.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorDuplicateNameAsPattern.um})
   (concurrent
    (diff? tokens/ErrorDuplicateNameAsPattern.expected
     tokens/ErrorDuplicateNameAsPattern.out)
    (diff? ast/ErrorDuplicateNameAsPattern.expected
     ast/ErrorDuplicateNameAsPattern.out)
    (diff? mir/ErrorDuplicateNameAsPattern.expected
     mir/ErrorDuplicateNameAsPattern.out)
    (diff? llvm/ErrorDuplicateNameAsPattern.expected
     llvm/ErrorDuplicateNameAsPattern.out)
    (diff? output/ErrorDuplicateNameAsPattern.expected
     output/ErrorDuplicateNameAsPattern.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorDuplicateTypeParameter.um})
   (concurrent
    (diff? tokens/ErrorDuplicateTypeParameter.expected
     tokens/ErrorDuplicateTypeParameter.out)
    (diff? ast/ErrorDuplicateTypeParameter.expected
     ast/ErrorDuplicateTypeParameter.out)
    (diff? mir/ErrorDuplicateTypeParameter.expected
     mir/ErrorDuplicateTypeParameter.out)
    (diff? llvm/ErrorDuplicateTypeParameter.expected
     llvm/ErrorDuplicateTypeParameter.out)
    (diff? output/ErrorDuplicateTypeParameter.expected
     output/ErrorDuplicateTypeParameter.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorDuplicateVal.um})
   (concurrent
    (diff? tokens/ErrorDuplicateVal.expected tokens/ErrorDuplicateVal.out)
    (diff? ast/ErrorDuplicateVal.expected ast/ErrorDuplicateVal.out)
    (diff? mir/ErrorDuplicateVal.expected mir/ErrorDuplicateVal.out)
    (diff? llvm/ErrorDuplicateVal.expected llvm/ErrorDuplicateVal.out)
    (diff? output/ErrorDuplicateVal.expected output/ErrorDuplicateVal.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorEffectOpUnboundParams.um})
   (concurrent
    (diff? tokens/ErrorEffectOpUnboundParams.expected
     tokens/ErrorEffectOpUnboundParams.out)
    (diff? ast/ErrorEffectOpUnboundParams.expected
     ast/ErrorEffectOpUnboundParams.out)
    (diff? mir/ErrorEffectOpUnboundParams.expected
     mir/ErrorEffectOpUnboundParams.out)
    (diff? llvm/ErrorEffectOpUnboundParams.expected
     llvm/ErrorEffectOpUnboundParams.out)
    (diff? output/ErrorEffectOpUnboundParams.expected
     output/ErrorEffectOpUnboundParams.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorEffectOpValClash.um})
   (concurrent
    (diff? tokens/ErrorEffectOpValClash.expected
     tokens/ErrorEffectOpValClash.out)
    (diff? ast/ErrorEffectOpValClash.expected ast/ErrorEffectOpValClash.out)
    (diff? mir/ErrorEffectOpValClash.expected mir/ErrorEffectOpValClash.out)
    (diff? llvm/ErrorEffectOpValClash.expected
     llvm/ErrorEffectOpValClash.out)
    (diff? output/ErrorEffectOpValClash.expected
     output/ErrorEffectOpValClash.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorEffectSigDefDiff.um})
   (concurrent
    (diff? tokens/ErrorEffectSigDefDiff.expected
     tokens/ErrorEffectSigDefDiff.out)
    (diff? ast/ErrorEffectSigDefDiff.expected ast/ErrorEffectSigDefDiff.out)
    (diff? mir/ErrorEffectSigDefDiff.expected mir/ErrorEffectSigDefDiff.out)
    (diff? llvm/ErrorEffectSigDefDiff.expected
     llvm/ErrorEffectSigDefDiff.out)
    (diff? output/ErrorEffectSigDefDiff.expected
     output/ErrorEffectSigDefDiff.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorEffectsNoValueBranch.um})
   (concurrent
    (diff? tokens/ErrorEffectsNoValueBranch.expected
     tokens/ErrorEffectsNoValueBranch.out)
    (diff? ast/ErrorEffectsNoValueBranch.expected
     ast/ErrorEffectsNoValueBranch.out)
    (diff? mir/ErrorEffectsNoValueBranch.expected
     mir/ErrorEffectsNoValueBranch.out)
    (diff? llvm/ErrorEffectsNoValueBranch.expected
     llvm/ErrorEffectsNoValueBranch.out)
    (diff? output/ErrorEffectsNoValueBranch.expected
     output/ErrorEffectsNoValueBranch.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorHandleNonOperation.um})
   (concurrent
    (diff? tokens/ErrorHandleNonOperation.expected
     tokens/ErrorHandleNonOperation.out)
    (diff? ast/ErrorHandleNonOperation.expected
     ast/ErrorHandleNonOperation.out)
    (diff? mir/ErrorHandleNonOperation.expected
     mir/ErrorHandleNonOperation.out)
    (diff? llvm/ErrorHandleNonOperation.expected
     llvm/ErrorHandleNonOperation.out)
    (diff? output/ErrorHandleNonOperation.expected
     output/ErrorHandleNonOperation.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorHangingVal.um})
   (concurrent
    (diff? tokens/ErrorHangingVal.expected tokens/ErrorHangingVal.out)
    (diff? ast/ErrorHangingVal.expected ast/ErrorHangingVal.out)
    (diff? mir/ErrorHangingVal.expected mir/ErrorHangingVal.out)
    (diff? llvm/ErrorHangingVal.expected llvm/ErrorHangingVal.out)
    (diff? output/ErrorHangingVal.expected output/ErrorHangingVal.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorImportOfImport.um})
   (concurrent
    (diff? tokens/ErrorImportOfImport.expected
     tokens/ErrorImportOfImport.out)
    (diff? ast/ErrorImportOfImport.expected ast/ErrorImportOfImport.out)
    (diff? mir/ErrorImportOfImport.expected mir/ErrorImportOfImport.out)
    (diff? llvm/ErrorImportOfImport.expected llvm/ErrorImportOfImport.out)
    (diff? output/ErrorImportOfImport.expected
     output/ErrorImportOfImport.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorIncompatibleSig.um})
   (concurrent
    (diff? tokens/ErrorIncompatibleSig.expected
     tokens/ErrorIncompatibleSig.out)
    (diff? ast/ErrorIncompatibleSig.expected ast/ErrorIncompatibleSig.out)
    (diff? mir/ErrorIncompatibleSig.expected mir/ErrorIncompatibleSig.out)
    (diff? llvm/ErrorIncompatibleSig.expected llvm/ErrorIncompatibleSig.out)
    (diff? output/ErrorIncompatibleSig.expected
     output/ErrorIncompatibleSig.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorInconsistentValType.um})
   (concurrent
    (diff? tokens/ErrorInconsistentValType.expected
     tokens/ErrorInconsistentValType.out)
    (diff? ast/ErrorInconsistentValType.expected
     ast/ErrorInconsistentValType.out)
    (diff? mir/ErrorInconsistentValType.expected
     mir/ErrorInconsistentValType.out)
    (diff? llvm/ErrorInconsistentValType.expected
     llvm/ErrorInconsistentValType.out)
    (diff? output/ErrorInconsistentValType.expected
     output/ErrorInconsistentValType.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorInexhaustivePattern.um})
   (concurrent
    (diff? tokens/ErrorInexhaustivePattern.expected
     tokens/ErrorInexhaustivePattern.out)
    (diff? ast/ErrorInexhaustivePattern.expected
     ast/ErrorInexhaustivePattern.out)
    (diff? mir/ErrorInexhaustivePattern.expected
     mir/ErrorInexhaustivePattern.out)
    (diff? llvm/ErrorInexhaustivePattern.expected
     llvm/ErrorInexhaustivePattern.out)
    (diff? output/ErrorInexhaustivePattern.expected
     output/ErrorInexhaustivePattern.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorLetRecExpression.um})
   (concurrent
    (diff? tokens/ErrorLetRecExpression.expected
     tokens/ErrorLetRecExpression.out)
    (diff? ast/ErrorLetRecExpression.expected ast/ErrorLetRecExpression.out)
    (diff? mir/ErrorLetRecExpression.expected mir/ErrorLetRecExpression.out)
    (diff? llvm/ErrorLetRecExpression.expected
     llvm/ErrorLetRecExpression.out)
    (diff? output/ErrorLetRecExpression.expected
     output/ErrorLetRecExpression.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorNameClashExtern.um})
   (concurrent
    (diff? tokens/ErrorNameClashExtern.expected
     tokens/ErrorNameClashExtern.out)
    (diff? ast/ErrorNameClashExtern.expected ast/ErrorNameClashExtern.out)
    (diff? mir/ErrorNameClashExtern.expected mir/ErrorNameClashExtern.out)
    (diff? llvm/ErrorNameClashExtern.expected llvm/ErrorNameClashExtern.out)
    (diff? output/ErrorNameClashExtern.expected
     output/ErrorNameClashExtern.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorNameClashExternVal.um})
   (concurrent
    (diff? tokens/ErrorNameClashExternVal.expected
     tokens/ErrorNameClashExternVal.out)
    (diff? ast/ErrorNameClashExternVal.expected
     ast/ErrorNameClashExternVal.out)
    (diff? mir/ErrorNameClashExternVal.expected
     mir/ErrorNameClashExternVal.out)
    (diff? llvm/ErrorNameClashExternVal.expected
     llvm/ErrorNameClashExternVal.out)
    (diff? output/ErrorNameClashExternVal.expected
     output/ErrorNameClashExternVal.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorNameClashWithImport.um})
   (concurrent
    (diff? tokens/ErrorNameClashWithImport.expected
     tokens/ErrorNameClashWithImport.out)
    (diff? ast/ErrorNameClashWithImport.expected
     ast/ErrorNameClashWithImport.out)
    (diff? mir/ErrorNameClashWithImport.expected
     mir/ErrorNameClashWithImport.out)
    (diff? llvm/ErrorNameClashWithImport.expected
     llvm/ErrorNameClashWithImport.out)
    (diff? output/ErrorNameClashWithImport.expected
     output/ErrorNameClashWithImport.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorNeverSubtyping.um})
   (concurrent
    (diff? tokens/ErrorNeverSubtyping.expected
     tokens/ErrorNeverSubtyping.out)
    (diff? ast/ErrorNeverSubtyping.expected ast/ErrorNeverSubtyping.out)
    (diff? mir/ErrorNeverSubtyping.expected mir/ErrorNeverSubtyping.out)
    (diff? llvm/ErrorNeverSubtyping.expected llvm/ErrorNeverSubtyping.out)
    (diff? output/ErrorNeverSubtyping.expected
     output/ErrorNeverSubtyping.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorPartialApplication.um})
   (concurrent
    (diff? tokens/ErrorPartialApplication.expected
     tokens/ErrorPartialApplication.out)
    (diff? ast/ErrorPartialApplication.expected
     ast/ErrorPartialApplication.out)
    (diff? mir/ErrorPartialApplication.expected
     mir/ErrorPartialApplication.out)
    (diff? llvm/ErrorPartialApplication.expected
     llvm/ErrorPartialApplication.out)
    (diff? output/ErrorPartialApplication.expected
     output/ErrorPartialApplication.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorPatternUnionNameTypes.um})
   (concurrent
    (diff? tokens/ErrorPatternUnionNameTypes.expected
     tokens/ErrorPatternUnionNameTypes.out)
    (diff? ast/ErrorPatternUnionNameTypes.expected
     ast/ErrorPatternUnionNameTypes.out)
    (diff? mir/ErrorPatternUnionNameTypes.expected
     mir/ErrorPatternUnionNameTypes.out)
    (diff? llvm/ErrorPatternUnionNameTypes.expected
     llvm/ErrorPatternUnionNameTypes.out)
    (diff? output/ErrorPatternUnionNameTypes.expected
     output/ErrorPatternUnionNameTypes.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorPolymorphicRecursion.um})
   (concurrent
    (diff? tokens/ErrorPolymorphicRecursion.expected
     tokens/ErrorPolymorphicRecursion.out)
    (diff? ast/ErrorPolymorphicRecursion.expected
     ast/ErrorPolymorphicRecursion.out)
    (diff? mir/ErrorPolymorphicRecursion.expected
     mir/ErrorPolymorphicRecursion.out)
    (diff? llvm/ErrorPolymorphicRecursion.expected
     llvm/ErrorPolymorphicRecursion.out)
    (diff? output/ErrorPolymorphicRecursion.expected
     output/ErrorPolymorphicRecursion.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorResumeEffects.um})
   (concurrent
    (diff? tokens/ErrorResumeEffects.expected tokens/ErrorResumeEffects.out)
    (diff? ast/ErrorResumeEffects.expected ast/ErrorResumeEffects.out)
    (diff? mir/ErrorResumeEffects.expected mir/ErrorResumeEffects.out)
    (diff? llvm/ErrorResumeEffects.expected llvm/ErrorResumeEffects.out)
    (diff? output/ErrorResumeEffects.expected output/ErrorResumeEffects.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorShadowedBool.um})
   (concurrent
    (diff? tokens/ErrorShadowedBool.expected tokens/ErrorShadowedBool.out)
    (diff? ast/ErrorShadowedBool.expected ast/ErrorShadowedBool.out)
    (diff? mir/ErrorShadowedBool.expected mir/ErrorShadowedBool.out)
    (diff? llvm/ErrorShadowedBool.expected llvm/ErrorShadowedBool.out)
    (diff? output/ErrorShadowedBool.expected output/ErrorShadowedBool.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorSignatureCompatibility.um})
   (concurrent
    (diff? tokens/ErrorSignatureCompatibility.expected
     tokens/ErrorSignatureCompatibility.out)
    (diff? ast/ErrorSignatureCompatibility.expected
     ast/ErrorSignatureCompatibility.out)
    (diff? mir/ErrorSignatureCompatibility.expected
     mir/ErrorSignatureCompatibility.out)
    (diff? llvm/ErrorSignatureCompatibility.expected
     llvm/ErrorSignatureCompatibility.out)
    (diff? output/ErrorSignatureCompatibility.expected
     output/ErrorSignatureCompatibility.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorSubmoduleSignatures.um})
   (concurrent
    (diff? tokens/ErrorSubmoduleSignatures.expected
     tokens/ErrorSubmoduleSignatures.out)
    (diff? ast/ErrorSubmoduleSignatures.expected
     ast/ErrorSubmoduleSignatures.out)
    (diff? mir/ErrorSubmoduleSignatures.expected
     mir/ErrorSubmoduleSignatures.out)
    (diff? llvm/ErrorSubmoduleSignatures.expected
     llvm/ErrorSubmoduleSignatures.out)
    (diff? output/ErrorSubmoduleSignatures.expected
     output/ErrorSubmoduleSignatures.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorToplevelImport.um})
   (concurrent
    (diff? tokens/ErrorToplevelImport.expected
     tokens/ErrorToplevelImport.out)
    (diff? ast/ErrorToplevelImport.expected ast/ErrorToplevelImport.out)
    (diff? mir/ErrorToplevelImport.expected mir/ErrorToplevelImport.out)
    (diff? llvm/ErrorToplevelImport.expected llvm/ErrorToplevelImport.out)
    (diff? output/ErrorToplevelImport.expected
     output/ErrorToplevelImport.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ErrorTypeDeclFreeParams.um})
   (concurrent
    (diff? tokens/ErrorTypeDeclFreeParams.expected
     tokens/ErrorTypeDeclFreeParams.out)
    (diff? ast/ErrorTypeDeclFreeParams.expected
     ast/ErrorTypeDeclFreeParams.out)
    (diff? mir/ErrorTypeDeclFreeParams.expected
     mir/ErrorTypeDeclFreeParams.out)
    (diff? llvm/ErrorTypeDeclFreeParams.expected
     llvm/ErrorTypeDeclFreeParams.out)
    (diff? output/ErrorTypeDeclFreeParams.expected
     output/ErrorTypeDeclFreeParams.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn
   (run %{dep:test.exe} %{dep:examples/ErrorUnqualifiedUniversalImport.um})
   (concurrent
    (diff? tokens/ErrorUnqualifiedUniversalImport.expected
     tokens/ErrorUnqualifiedUniversalImport.out)
    (diff? ast/ErrorUnqualifiedUniversalImport.expected
     ast/ErrorUnqualifiedUniversalImport.out)
    (diff? mir/ErrorUnqualifiedUniversalImport.expected
     mir/ErrorUnqualifiedUniversalImport.out)
    (diff? llvm/ErrorUnqualifiedUniversalImport.expected
     llvm/ErrorUnqualifiedUniversalImport.out)
    (diff? output/ErrorUnqualifiedUniversalImport.expected
     output/ErrorUnqualifiedUniversalImport.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ExternInt.um})
   (concurrent (diff? tokens/ExternInt.expected tokens/ExternInt.out)
    (diff? ast/ExternInt.expected ast/ExternInt.out)
    (diff? mir/ExternInt.expected mir/ExternInt.out)
    (diff? llvm/ExternInt.expected llvm/ExternInt.out)
    (diff? output/ExternInt.expected output/ExternInt.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Functions.um})
   (concurrent (diff? tokens/Functions.expected tokens/Functions.out)
    (diff? ast/Functions.expected ast/Functions.out)
    (diff? mir/Functions.expected mir/Functions.out)
    (diff? llvm/Functions.expected llvm/Functions.out)
    (diff? output/Functions.expected output/Functions.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Generalization.um})
   (concurrent
    (diff? tokens/Generalization.expected tokens/Generalization.out)
    (diff? ast/Generalization.expected ast/Generalization.out)
    (diff? mir/Generalization.expected mir/Generalization.out)
    (diff? llvm/Generalization.expected llvm/Generalization.out)
    (diff? output/Generalization.expected output/Generalization.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/HelloWorld.um})
   (concurrent (diff? tokens/HelloWorld.expected tokens/HelloWorld.out)
    (diff? ast/HelloWorld.expected ast/HelloWorld.out)
    (diff? mir/HelloWorld.expected mir/HelloWorld.out)
    (diff? llvm/HelloWorld.expected llvm/HelloWorld.out)
    (diff? output/HelloWorld.expected output/HelloWorld.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Imports.um})
   (concurrent (diff? tokens/Imports.expected tokens/Imports.out)
    (diff? ast/Imports.expected ast/Imports.out)
    (diff? mir/Imports.expected mir/Imports.out)
    (diff? llvm/Imports.expected llvm/Imports.out)
    (diff? output/Imports.expected output/Imports.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/LetBindingGroups.um})
   (concurrent
    (diff? tokens/LetBindingGroups.expected tokens/LetBindingGroups.out)
    (diff? ast/LetBindingGroups.expected ast/LetBindingGroups.out)
    (diff? mir/LetBindingGroups.expected mir/LetBindingGroups.out)
    (diff? llvm/LetBindingGroups.expected llvm/LetBindingGroups.out)
    (diff? output/LetBindingGroups.expected output/LetBindingGroups.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/LetPattern.um})
   (concurrent (diff? tokens/LetPattern.expected tokens/LetPattern.out)
    (diff? ast/LetPattern.expected ast/LetPattern.out)
    (diff? mir/LetPattern.expected mir/LetPattern.out)
    (diff? llvm/LetPattern.expected llvm/LetPattern.out)
    (diff? output/LetPattern.expected output/LetPattern.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ModuleSig.um})
   (concurrent (diff? tokens/ModuleSig.expected tokens/ModuleSig.out)
    (diff? ast/ModuleSig.expected ast/ModuleSig.out)
    (diff? mir/ModuleSig.expected mir/ModuleSig.out)
    (diff? llvm/ModuleSig.expected llvm/ModuleSig.out)
    (diff? output/ModuleSig.expected output/ModuleSig.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Modules.um})
   (concurrent (diff? tokens/Modules.expected tokens/Modules.out)
    (diff? ast/Modules.expected ast/Modules.out)
    (diff? mir/Modules.expected mir/Modules.out)
    (diff? llvm/Modules.expected llvm/Modules.out)
    (diff? output/Modules.expected output/Modules.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/MoreEffectsInSig.um})
   (concurrent
    (diff? tokens/MoreEffectsInSig.expected tokens/MoreEffectsInSig.out)
    (diff? ast/MoreEffectsInSig.expected ast/MoreEffectsInSig.out)
    (diff? mir/MoreEffectsInSig.expected mir/MoreEffectsInSig.out)
    (diff? llvm/MoreEffectsInSig.expected llvm/MoreEffectsInSig.out)
    (diff? output/MoreEffectsInSig.expected output/MoreEffectsInSig.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/MutualRecursion.um})
   (concurrent
    (diff? tokens/MutualRecursion.expected tokens/MutualRecursion.out)
    (diff? ast/MutualRecursion.expected ast/MutualRecursion.out)
    (diff? mir/MutualRecursion.expected mir/MutualRecursion.out)
    (diff? llvm/MutualRecursion.expected llvm/MutualRecursion.out)
    (diff? output/MutualRecursion.expected output/MutualRecursion.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/NeverAndAny.um})
   (concurrent (diff? tokens/NeverAndAny.expected tokens/NeverAndAny.out)
    (diff? ast/NeverAndAny.expected ast/NeverAndAny.out)
    (diff? mir/NeverAndAny.expected mir/NeverAndAny.out)
    (diff? llvm/NeverAndAny.expected llvm/NeverAndAny.out)
    (diff? output/NeverAndAny.expected output/NeverAndAny.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/OpSectionLeft.um})
   (concurrent (diff? tokens/OpSectionLeft.expected tokens/OpSectionLeft.out)
    (diff? ast/OpSectionLeft.expected ast/OpSectionLeft.out)
    (diff? mir/OpSectionLeft.expected mir/OpSectionLeft.out)
    (diff? llvm/OpSectionLeft.expected llvm/OpSectionLeft.out)
    (diff? output/OpSectionLeft.expected output/OpSectionLeft.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/OpSectionRight.um})
   (concurrent
    (diff? tokens/OpSectionRight.expected tokens/OpSectionRight.out)
    (diff? ast/OpSectionRight.expected ast/OpSectionRight.out)
    (diff? mir/OpSectionRight.expected mir/OpSectionRight.out)
    (diff? llvm/OpSectionRight.expected llvm/OpSectionRight.out)
    (diff? output/OpSectionRight.expected output/OpSectionRight.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Operators.um})
   (concurrent (diff? tokens/Operators.expected tokens/Operators.out)
    (diff? ast/Operators.expected ast/Operators.out)
    (diff? mir/Operators.expected mir/Operators.out)
    (diff? llvm/Operators.expected llvm/Operators.out)
    (diff? output/Operators.expected output/Operators.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Option.um})
   (concurrent (diff? tokens/Option.expected tokens/Option.out)
    (diff? ast/Option.expected ast/Option.out)
    (diff? mir/Option.expected mir/Option.out)
    (diff? llvm/Option.expected llvm/Option.out)
    (diff? output/Option.expected output/Option.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/OverlappingImports.um})
   (concurrent
    (diff? tokens/OverlappingImports.expected tokens/OverlappingImports.out)
    (diff? ast/OverlappingImports.expected ast/OverlappingImports.out)
    (diff? mir/OverlappingImports.expected mir/OverlappingImports.out)
    (diff? llvm/OverlappingImports.expected llvm/OverlappingImports.out)
    (diff? output/OverlappingImports.expected output/OverlappingImports.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Paths.um})
   (concurrent (diff? tokens/Paths.expected tokens/Paths.out)
    (diff? ast/Paths.expected ast/Paths.out)
    (diff? mir/Paths.expected mir/Paths.out)
    (diff? llvm/Paths.expected llvm/Paths.out)
    (diff? output/Paths.expected output/Paths.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/RecursiveEffectDecl.um})
   (concurrent
    (diff? tokens/RecursiveEffectDecl.expected
     tokens/RecursiveEffectDecl.out)
    (diff? ast/RecursiveEffectDecl.expected ast/RecursiveEffectDecl.out)
    (diff? mir/RecursiveEffectDecl.expected mir/RecursiveEffectDecl.out)
    (diff? llvm/RecursiveEffectDecl.expected llvm/RecursiveEffectDecl.out)
    (diff? output/RecursiveEffectDecl.expected
     output/RecursiveEffectDecl.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/ReraiseExn.um})
   (concurrent (diff? tokens/ReraiseExn.expected tokens/ReraiseExn.out)
    (diff? ast/ReraiseExn.expected ast/ReraiseExn.out)
    (diff? mir/ReraiseExn.expected mir/ReraiseExn.out)
    (diff? llvm/ReraiseExn.expected llvm/ReraiseExn.out)
    (diff? output/ReraiseExn.expected output/ReraiseExn.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/SelfReferentialImport.um})
   (concurrent
    (diff? tokens/SelfReferentialImport.expected
     tokens/SelfReferentialImport.out)
    (diff? ast/SelfReferentialImport.expected ast/SelfReferentialImport.out)
    (diff? mir/SelfReferentialImport.expected mir/SelfReferentialImport.out)
    (diff? llvm/SelfReferentialImport.expected
     llvm/SelfReferentialImport.out)
    (diff? output/SelfReferentialImport.expected
     output/SelfReferentialImport.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Text.um})
   (concurrent (diff? tokens/Text.expected tokens/Text.out)
    (diff? ast/Text.expected ast/Text.out)
    (diff? mir/Text.expected mir/Text.out)
    (diff? llvm/Text.expected llvm/Text.out)
    (diff? output/Text.expected output/Text.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Traits.um})
   (concurrent (diff? tokens/Traits.expected tokens/Traits.out)
    (diff? ast/Traits.expected ast/Traits.out)
    (diff? mir/Traits.expected mir/Traits.out)
    (diff? llvm/Traits.expected llvm/Traits.out)
    (diff? output/Traits.expected output/Traits.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/TypeChecking.um})
   (concurrent (diff? tokens/TypeChecking.expected tokens/TypeChecking.out)
    (diff? ast/TypeChecking.expected ast/TypeChecking.out)
    (diff? mir/TypeChecking.expected mir/TypeChecking.out)
    (diff? llvm/TypeChecking.expected llvm/TypeChecking.out)
    (diff? output/TypeChecking.expected output/TypeChecking.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/TypeModuleNamePunning.um})
   (concurrent
    (diff? tokens/TypeModuleNamePunning.expected
     tokens/TypeModuleNamePunning.out)
    (diff? ast/TypeModuleNamePunning.expected ast/TypeModuleNamePunning.out)
    (diff? mir/TypeModuleNamePunning.expected mir/TypeModuleNamePunning.out)
    (diff? llvm/TypeModuleNamePunning.expected
     llvm/TypeModuleNamePunning.out)
    (diff? output/TypeModuleNamePunning.expected
     output/TypeModuleNamePunning.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/Types.um})
   (concurrent (diff? tokens/Types.expected tokens/Types.out)
    (diff? ast/Types.expected ast/Types.out)
    (diff? mir/Types.expected mir/Types.out)
    (diff? llvm/Types.expected llvm/Types.out)
    (diff? output/Types.expected output/Types.out)))))
(rule (alias runtest) (deps (package umber))
 (action
  (progn (run %{dep:test.exe} %{dep:examples/WeirdFunCalls.um})
   (concurrent (diff? tokens/WeirdFunCalls.expected tokens/WeirdFunCalls.out)
    (diff? ast/WeirdFunCalls.expected ast/WeirdFunCalls.out)
    (diff? mir/WeirdFunCalls.expected mir/WeirdFunCalls.out)
    (diff? llvm/WeirdFunCalls.expected llvm/WeirdFunCalls.out)
    (diff? output/WeirdFunCalls.expected output/WeirdFunCalls.out)))))
