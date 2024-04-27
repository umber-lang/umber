(rule (alias runtest)
 (deps (package umber) (glob_files examples/*) (glob_files tokens/*.expected)
  (glob_files ast/*.expected) (glob_files mir/*.expected)
  (glob_files llvm/*.expected) (glob_files output/*.expected))
 (action
  (progn (run %{dep:test.exe})
   (concurrent (diff? tokens/AsPattern.expected tokens/AsPattern.out)
    (diff? tokens/Basics.expected tokens/Basics.out)
    (diff? tokens/Classics.expected tokens/Classics.out)
    (diff? tokens/Closures.expected tokens/Closures.out)
    (diff? tokens/Constructors.expected tokens/Constructors.out)
    (diff? tokens/CrossModuleUsage.expected tokens/CrossModuleUsage.out)
    (diff? tokens/Effects.expected tokens/Effects.out)
    (diff? tokens/Empty.expected tokens/Empty.out)
    (diff? tokens/ErrorCrossModuleMutualRecursion.expected
     tokens/ErrorCrossModuleMutualRecursion.out)
    (diff? tokens/ErrorCyclicTypeAlias.expected
     tokens/ErrorCyclicTypeAlias.out)
    (diff? tokens/ErrorDuplicateEffectTypeParameter.expected
     tokens/ErrorDuplicateEffectTypeParameter.out)
    (diff? tokens/ErrorDuplicateLet.expected tokens/ErrorDuplicateLet.out)
    (diff? tokens/ErrorDuplicateNameAsPattern.expected
     tokens/ErrorDuplicateNameAsPattern.out)
    (diff? tokens/ErrorDuplicateTypeParameter.expected
     tokens/ErrorDuplicateTypeParameter.out)
    (diff? tokens/ErrorDuplicateVal.expected tokens/ErrorDuplicateVal.out)
    (diff? tokens/ErrorEffectOpUnboundParams.expected
     tokens/ErrorEffectOpUnboundParams.out)
    (diff? tokens/ErrorEffectSigDefDiff.expected
     tokens/ErrorEffectSigDefDiff.out)
    (diff? tokens/ErrorEffectsNoValueBranch.expected
     tokens/ErrorEffectsNoValueBranch.out)
    (diff? tokens/ErrorHangingVal.expected tokens/ErrorHangingVal.out)
    (diff? tokens/ErrorImportOfImport.expected
     tokens/ErrorImportOfImport.out)
    (diff? tokens/ErrorIncompatibleSig.expected
     tokens/ErrorIncompatibleSig.out)
    (diff? tokens/ErrorInconsistentValType.expected
     tokens/ErrorInconsistentValType.out)
    (diff? tokens/ErrorInexhaustivePattern.expected
     tokens/ErrorInexhaustivePattern.out)
    (diff? tokens/ErrorLetRecExpression.expected
     tokens/ErrorLetRecExpression.out)
    (diff? tokens/ErrorNameClashExtern.expected
     tokens/ErrorNameClashExtern.out)
    (diff? tokens/ErrorNameClashExternVal.expected
     tokens/ErrorNameClashExternVal.out)
    (diff? tokens/ErrorNameClashWithImport.expected
     tokens/ErrorNameClashWithImport.out)
    (diff? tokens/ErrorPartialApplication.expected
     tokens/ErrorPartialApplication.out)
    (diff? tokens/ErrorPatternUnionNameTypes.expected
     tokens/ErrorPatternUnionNameTypes.out)
    (diff? tokens/ErrorPolymorphicRecursion.expected
     tokens/ErrorPolymorphicRecursion.out)
    (diff? tokens/ErrorResumeEffects.expected tokens/ErrorResumeEffects.out)
    (diff? tokens/ErrorShadowedBool.expected tokens/ErrorShadowedBool.out)
    (diff? tokens/ErrorSignatureCompatibility.expected
     tokens/ErrorSignatureCompatibility.out)
    (diff? tokens/ErrorSubmoduleSignatures.expected
     tokens/ErrorSubmoduleSignatures.out)
    (diff? tokens/ErrorToplevelImport.expected
     tokens/ErrorToplevelImport.out)
    (diff? tokens/ErrorTypeDeclFreeParams.expected
     tokens/ErrorTypeDeclFreeParams.out)
    (diff? tokens/ErrorUnqualifiedUniversalImport.expected
     tokens/ErrorUnqualifiedUniversalImport.out)
    (diff? tokens/ExternInt.expected tokens/ExternInt.out)
    (diff? tokens/Functions.expected tokens/Functions.out)
    (diff? tokens/Generalization.expected tokens/Generalization.out)
    (diff? tokens/HelloWorld.expected tokens/HelloWorld.out)
    (diff? tokens/Imports.expected tokens/Imports.out)
    (diff? tokens/LetBindingGroups.expected tokens/LetBindingGroups.out)
    (diff? tokens/LetPattern.expected tokens/LetPattern.out)
    (diff? tokens/ModuleSig.expected tokens/ModuleSig.out)
    (diff? tokens/Modules.expected tokens/Modules.out)
    (diff? tokens/MoreEffectsInSig.expected tokens/MoreEffectsInSig.out)
    (diff? tokens/MutualRecursion.expected tokens/MutualRecursion.out)
    (diff? tokens/OpSectionLeft.expected tokens/OpSectionLeft.out)
    (diff? tokens/OpSectionRight.expected tokens/OpSectionRight.out)
    (diff? tokens/Operators.expected tokens/Operators.out)
    (diff? tokens/Option.expected tokens/Option.out)
    (diff? tokens/OverlappingImports.expected tokens/OverlappingImports.out)
    (diff? tokens/Paths.expected tokens/Paths.out)
    (diff? tokens/RecursiveEffectDecl.expected
     tokens/RecursiveEffectDecl.out)
    (diff? tokens/SelfReferentialImport.expected
     tokens/SelfReferentialImport.out)
    (diff? tokens/Text.expected tokens/Text.out)
    (diff? tokens/Traits.expected tokens/Traits.out)
    (diff? tokens/TypeChecking.expected tokens/TypeChecking.out)
    (diff? tokens/TypeModuleNamePunning.expected
     tokens/TypeModuleNamePunning.out)
    (diff? tokens/Types.expected tokens/Types.out)
    (diff? tokens/WeirdFunCalls.expected tokens/WeirdFunCalls.out)
    (diff? ast/AsPattern.expected ast/AsPattern.out)
    (diff? ast/Basics.expected ast/Basics.out)
    (diff? ast/Classics.expected ast/Classics.out)
    (diff? ast/Closures.expected ast/Closures.out)
    (diff? ast/Constructors.expected ast/Constructors.out)
    (diff? ast/CrossModuleUsage.expected ast/CrossModuleUsage.out)
    (diff? ast/Effects.expected ast/Effects.out)
    (diff? ast/Empty.expected ast/Empty.out)
    (diff? ast/ErrorCrossModuleMutualRecursion.expected
     ast/ErrorCrossModuleMutualRecursion.out)
    (diff? ast/ErrorCyclicTypeAlias.expected ast/ErrorCyclicTypeAlias.out)
    (diff? ast/ErrorDuplicateEffectTypeParameter.expected
     ast/ErrorDuplicateEffectTypeParameter.out)
    (diff? ast/ErrorDuplicateLet.expected ast/ErrorDuplicateLet.out)
    (diff? ast/ErrorDuplicateNameAsPattern.expected
     ast/ErrorDuplicateNameAsPattern.out)
    (diff? ast/ErrorDuplicateTypeParameter.expected
     ast/ErrorDuplicateTypeParameter.out)
    (diff? ast/ErrorDuplicateVal.expected ast/ErrorDuplicateVal.out)
    (diff? ast/ErrorEffectOpUnboundParams.expected
     ast/ErrorEffectOpUnboundParams.out)
    (diff? ast/ErrorEffectSigDefDiff.expected ast/ErrorEffectSigDefDiff.out)
    (diff? ast/ErrorEffectsNoValueBranch.expected
     ast/ErrorEffectsNoValueBranch.out)
    (diff? ast/ErrorHangingVal.expected ast/ErrorHangingVal.out)
    (diff? ast/ErrorImportOfImport.expected ast/ErrorImportOfImport.out)
    (diff? ast/ErrorIncompatibleSig.expected ast/ErrorIncompatibleSig.out)
    (diff? ast/ErrorInconsistentValType.expected
     ast/ErrorInconsistentValType.out)
    (diff? ast/ErrorInexhaustivePattern.expected
     ast/ErrorInexhaustivePattern.out)
    (diff? ast/ErrorLetRecExpression.expected ast/ErrorLetRecExpression.out)
    (diff? ast/ErrorNameClashExtern.expected ast/ErrorNameClashExtern.out)
    (diff? ast/ErrorNameClashExternVal.expected
     ast/ErrorNameClashExternVal.out)
    (diff? ast/ErrorNameClashWithImport.expected
     ast/ErrorNameClashWithImport.out)
    (diff? ast/ErrorPartialApplication.expected
     ast/ErrorPartialApplication.out)
    (diff? ast/ErrorPatternUnionNameTypes.expected
     ast/ErrorPatternUnionNameTypes.out)
    (diff? ast/ErrorPolymorphicRecursion.expected
     ast/ErrorPolymorphicRecursion.out)
    (diff? ast/ErrorResumeEffects.expected ast/ErrorResumeEffects.out)
    (diff? ast/ErrorShadowedBool.expected ast/ErrorShadowedBool.out)
    (diff? ast/ErrorSignatureCompatibility.expected
     ast/ErrorSignatureCompatibility.out)
    (diff? ast/ErrorSubmoduleSignatures.expected
     ast/ErrorSubmoduleSignatures.out)
    (diff? ast/ErrorToplevelImport.expected ast/ErrorToplevelImport.out)
    (diff? ast/ErrorTypeDeclFreeParams.expected
     ast/ErrorTypeDeclFreeParams.out)
    (diff? ast/ErrorUnqualifiedUniversalImport.expected
     ast/ErrorUnqualifiedUniversalImport.out)
    (diff? ast/ExternInt.expected ast/ExternInt.out)
    (diff? ast/Functions.expected ast/Functions.out)
    (diff? ast/Generalization.expected ast/Generalization.out)
    (diff? ast/HelloWorld.expected ast/HelloWorld.out)
    (diff? ast/Imports.expected ast/Imports.out)
    (diff? ast/LetBindingGroups.expected ast/LetBindingGroups.out)
    (diff? ast/LetPattern.expected ast/LetPattern.out)
    (diff? ast/ModuleSig.expected ast/ModuleSig.out)
    (diff? ast/Modules.expected ast/Modules.out)
    (diff? ast/MoreEffectsInSig.expected ast/MoreEffectsInSig.out)
    (diff? ast/MutualRecursion.expected ast/MutualRecursion.out)
    (diff? ast/OpSectionLeft.expected ast/OpSectionLeft.out)
    (diff? ast/OpSectionRight.expected ast/OpSectionRight.out)
    (diff? ast/Operators.expected ast/Operators.out)
    (diff? ast/Option.expected ast/Option.out)
    (diff? ast/OverlappingImports.expected ast/OverlappingImports.out)
    (diff? ast/Paths.expected ast/Paths.out)
    (diff? ast/RecursiveEffectDecl.expected ast/RecursiveEffectDecl.out)
    (diff? ast/SelfReferentialImport.expected ast/SelfReferentialImport.out)
    (diff? ast/Text.expected ast/Text.out)
    (diff? ast/Traits.expected ast/Traits.out)
    (diff? ast/TypeChecking.expected ast/TypeChecking.out)
    (diff? ast/TypeModuleNamePunning.expected ast/TypeModuleNamePunning.out)
    (diff? ast/Types.expected ast/Types.out)
    (diff? ast/WeirdFunCalls.expected ast/WeirdFunCalls.out)
    (diff? mir/AsPattern.expected mir/AsPattern.out)
    (diff? mir/Basics.expected mir/Basics.out)
    (diff? mir/Classics.expected mir/Classics.out)
    (diff? mir/Closures.expected mir/Closures.out)
    (diff? mir/Constructors.expected mir/Constructors.out)
    (diff? mir/CrossModuleUsage.expected mir/CrossModuleUsage.out)
    (diff? mir/Effects.expected mir/Effects.out)
    (diff? mir/Empty.expected mir/Empty.out)
    (diff? mir/ErrorCrossModuleMutualRecursion.expected
     mir/ErrorCrossModuleMutualRecursion.out)
    (diff? mir/ErrorCyclicTypeAlias.expected mir/ErrorCyclicTypeAlias.out)
    (diff? mir/ErrorDuplicateEffectTypeParameter.expected
     mir/ErrorDuplicateEffectTypeParameter.out)
    (diff? mir/ErrorDuplicateLet.expected mir/ErrorDuplicateLet.out)
    (diff? mir/ErrorDuplicateNameAsPattern.expected
     mir/ErrorDuplicateNameAsPattern.out)
    (diff? mir/ErrorDuplicateTypeParameter.expected
     mir/ErrorDuplicateTypeParameter.out)
    (diff? mir/ErrorDuplicateVal.expected mir/ErrorDuplicateVal.out)
    (diff? mir/ErrorEffectOpUnboundParams.expected
     mir/ErrorEffectOpUnboundParams.out)
    (diff? mir/ErrorEffectSigDefDiff.expected mir/ErrorEffectSigDefDiff.out)
    (diff? mir/ErrorEffectsNoValueBranch.expected
     mir/ErrorEffectsNoValueBranch.out)
    (diff? mir/ErrorHangingVal.expected mir/ErrorHangingVal.out)
    (diff? mir/ErrorImportOfImport.expected mir/ErrorImportOfImport.out)
    (diff? mir/ErrorIncompatibleSig.expected mir/ErrorIncompatibleSig.out)
    (diff? mir/ErrorInconsistentValType.expected
     mir/ErrorInconsistentValType.out)
    (diff? mir/ErrorInexhaustivePattern.expected
     mir/ErrorInexhaustivePattern.out)
    (diff? mir/ErrorLetRecExpression.expected mir/ErrorLetRecExpression.out)
    (diff? mir/ErrorNameClashExtern.expected mir/ErrorNameClashExtern.out)
    (diff? mir/ErrorNameClashExternVal.expected
     mir/ErrorNameClashExternVal.out)
    (diff? mir/ErrorNameClashWithImport.expected
     mir/ErrorNameClashWithImport.out)
    (diff? mir/ErrorPartialApplication.expected
     mir/ErrorPartialApplication.out)
    (diff? mir/ErrorPatternUnionNameTypes.expected
     mir/ErrorPatternUnionNameTypes.out)
    (diff? mir/ErrorPolymorphicRecursion.expected
     mir/ErrorPolymorphicRecursion.out)
    (diff? mir/ErrorResumeEffects.expected mir/ErrorResumeEffects.out)
    (diff? mir/ErrorShadowedBool.expected mir/ErrorShadowedBool.out)
    (diff? mir/ErrorSignatureCompatibility.expected
     mir/ErrorSignatureCompatibility.out)
    (diff? mir/ErrorSubmoduleSignatures.expected
     mir/ErrorSubmoduleSignatures.out)
    (diff? mir/ErrorToplevelImport.expected mir/ErrorToplevelImport.out)
    (diff? mir/ErrorTypeDeclFreeParams.expected
     mir/ErrorTypeDeclFreeParams.out)
    (diff? mir/ErrorUnqualifiedUniversalImport.expected
     mir/ErrorUnqualifiedUniversalImport.out)
    (diff? mir/ExternInt.expected mir/ExternInt.out)
    (diff? mir/Functions.expected mir/Functions.out)
    (diff? mir/Generalization.expected mir/Generalization.out)
    (diff? mir/HelloWorld.expected mir/HelloWorld.out)
    (diff? mir/Imports.expected mir/Imports.out)
    (diff? mir/LetBindingGroups.expected mir/LetBindingGroups.out)
    (diff? mir/LetPattern.expected mir/LetPattern.out)
    (diff? mir/ModuleSig.expected mir/ModuleSig.out)
    (diff? mir/Modules.expected mir/Modules.out)
    (diff? mir/MoreEffectsInSig.expected mir/MoreEffectsInSig.out)
    (diff? mir/MutualRecursion.expected mir/MutualRecursion.out)
    (diff? mir/OpSectionLeft.expected mir/OpSectionLeft.out)
    (diff? mir/OpSectionRight.expected mir/OpSectionRight.out)
    (diff? mir/Operators.expected mir/Operators.out)
    (diff? mir/Option.expected mir/Option.out)
    (diff? mir/OverlappingImports.expected mir/OverlappingImports.out)
    (diff? mir/Paths.expected mir/Paths.out)
    (diff? mir/RecursiveEffectDecl.expected mir/RecursiveEffectDecl.out)
    (diff? mir/SelfReferentialImport.expected mir/SelfReferentialImport.out)
    (diff? mir/Text.expected mir/Text.out)
    (diff? mir/Traits.expected mir/Traits.out)
    (diff? mir/TypeChecking.expected mir/TypeChecking.out)
    (diff? mir/TypeModuleNamePunning.expected mir/TypeModuleNamePunning.out)
    (diff? mir/Types.expected mir/Types.out)
    (diff? mir/WeirdFunCalls.expected mir/WeirdFunCalls.out)
    (diff? llvm/AsPattern.expected llvm/AsPattern.out)
    (diff? llvm/Basics.expected llvm/Basics.out)
    (diff? llvm/Classics.expected llvm/Classics.out)
    (diff? llvm/Closures.expected llvm/Closures.out)
    (diff? llvm/Constructors.expected llvm/Constructors.out)
    (diff? llvm/CrossModuleUsage.expected llvm/CrossModuleUsage.out)
    (diff? llvm/Effects.expected llvm/Effects.out)
    (diff? llvm/Empty.expected llvm/Empty.out)
    (diff? llvm/ErrorCrossModuleMutualRecursion.expected
     llvm/ErrorCrossModuleMutualRecursion.out)
    (diff? llvm/ErrorCyclicTypeAlias.expected llvm/ErrorCyclicTypeAlias.out)
    (diff? llvm/ErrorDuplicateEffectTypeParameter.expected
     llvm/ErrorDuplicateEffectTypeParameter.out)
    (diff? llvm/ErrorDuplicateLet.expected llvm/ErrorDuplicateLet.out)
    (diff? llvm/ErrorDuplicateNameAsPattern.expected
     llvm/ErrorDuplicateNameAsPattern.out)
    (diff? llvm/ErrorDuplicateTypeParameter.expected
     llvm/ErrorDuplicateTypeParameter.out)
    (diff? llvm/ErrorDuplicateVal.expected llvm/ErrorDuplicateVal.out)
    (diff? llvm/ErrorEffectOpUnboundParams.expected
     llvm/ErrorEffectOpUnboundParams.out)
    (diff? llvm/ErrorEffectSigDefDiff.expected
     llvm/ErrorEffectSigDefDiff.out)
    (diff? llvm/ErrorEffectsNoValueBranch.expected
     llvm/ErrorEffectsNoValueBranch.out)
    (diff? llvm/ErrorHangingVal.expected llvm/ErrorHangingVal.out)
    (diff? llvm/ErrorImportOfImport.expected llvm/ErrorImportOfImport.out)
    (diff? llvm/ErrorIncompatibleSig.expected llvm/ErrorIncompatibleSig.out)
    (diff? llvm/ErrorInconsistentValType.expected
     llvm/ErrorInconsistentValType.out)
    (diff? llvm/ErrorInexhaustivePattern.expected
     llvm/ErrorInexhaustivePattern.out)
    (diff? llvm/ErrorLetRecExpression.expected
     llvm/ErrorLetRecExpression.out)
    (diff? llvm/ErrorNameClashExtern.expected llvm/ErrorNameClashExtern.out)
    (diff? llvm/ErrorNameClashExternVal.expected
     llvm/ErrorNameClashExternVal.out)
    (diff? llvm/ErrorNameClashWithImport.expected
     llvm/ErrorNameClashWithImport.out)
    (diff? llvm/ErrorPartialApplication.expected
     llvm/ErrorPartialApplication.out)
    (diff? llvm/ErrorPatternUnionNameTypes.expected
     llvm/ErrorPatternUnionNameTypes.out)
    (diff? llvm/ErrorPolymorphicRecursion.expected
     llvm/ErrorPolymorphicRecursion.out)
    (diff? llvm/ErrorResumeEffects.expected llvm/ErrorResumeEffects.out)
    (diff? llvm/ErrorShadowedBool.expected llvm/ErrorShadowedBool.out)
    (diff? llvm/ErrorSignatureCompatibility.expected
     llvm/ErrorSignatureCompatibility.out)
    (diff? llvm/ErrorSubmoduleSignatures.expected
     llvm/ErrorSubmoduleSignatures.out)
    (diff? llvm/ErrorToplevelImport.expected llvm/ErrorToplevelImport.out)
    (diff? llvm/ErrorTypeDeclFreeParams.expected
     llvm/ErrorTypeDeclFreeParams.out)
    (diff? llvm/ErrorUnqualifiedUniversalImport.expected
     llvm/ErrorUnqualifiedUniversalImport.out)
    (diff? llvm/ExternInt.expected llvm/ExternInt.out)
    (diff? llvm/Functions.expected llvm/Functions.out)
    (diff? llvm/Generalization.expected llvm/Generalization.out)
    (diff? llvm/HelloWorld.expected llvm/HelloWorld.out)
    (diff? llvm/Imports.expected llvm/Imports.out)
    (diff? llvm/LetBindingGroups.expected llvm/LetBindingGroups.out)
    (diff? llvm/LetPattern.expected llvm/LetPattern.out)
    (diff? llvm/ModuleSig.expected llvm/ModuleSig.out)
    (diff? llvm/Modules.expected llvm/Modules.out)
    (diff? llvm/MoreEffectsInSig.expected llvm/MoreEffectsInSig.out)
    (diff? llvm/MutualRecursion.expected llvm/MutualRecursion.out)
    (diff? llvm/OpSectionLeft.expected llvm/OpSectionLeft.out)
    (diff? llvm/OpSectionRight.expected llvm/OpSectionRight.out)
    (diff? llvm/Operators.expected llvm/Operators.out)
    (diff? llvm/Option.expected llvm/Option.out)
    (diff? llvm/OverlappingImports.expected llvm/OverlappingImports.out)
    (diff? llvm/Paths.expected llvm/Paths.out)
    (diff? llvm/RecursiveEffectDecl.expected llvm/RecursiveEffectDecl.out)
    (diff? llvm/SelfReferentialImport.expected
     llvm/SelfReferentialImport.out)
    (diff? llvm/Text.expected llvm/Text.out)
    (diff? llvm/Traits.expected llvm/Traits.out)
    (diff? llvm/TypeChecking.expected llvm/TypeChecking.out)
    (diff? llvm/TypeModuleNamePunning.expected
     llvm/TypeModuleNamePunning.out)
    (diff? llvm/Types.expected llvm/Types.out)
    (diff? llvm/WeirdFunCalls.expected llvm/WeirdFunCalls.out)
    (diff? output/AsPattern.expected output/AsPattern.out)
    (diff? output/Basics.expected output/Basics.out)
    (diff? output/Classics.expected output/Classics.out)
    (diff? output/Closures.expected output/Closures.out)
    (diff? output/Constructors.expected output/Constructors.out)
    (diff? output/CrossModuleUsage.expected output/CrossModuleUsage.out)
    (diff? output/Effects.expected output/Effects.out)
    (diff? output/Empty.expected output/Empty.out)
    (diff? output/ErrorCrossModuleMutualRecursion.expected
     output/ErrorCrossModuleMutualRecursion.out)
    (diff? output/ErrorCyclicTypeAlias.expected
     output/ErrorCyclicTypeAlias.out)
    (diff? output/ErrorDuplicateEffectTypeParameter.expected
     output/ErrorDuplicateEffectTypeParameter.out)
    (diff? output/ErrorDuplicateLet.expected output/ErrorDuplicateLet.out)
    (diff? output/ErrorDuplicateNameAsPattern.expected
     output/ErrorDuplicateNameAsPattern.out)
    (diff? output/ErrorDuplicateTypeParameter.expected
     output/ErrorDuplicateTypeParameter.out)
    (diff? output/ErrorDuplicateVal.expected output/ErrorDuplicateVal.out)
    (diff? output/ErrorEffectOpUnboundParams.expected
     output/ErrorEffectOpUnboundParams.out)
    (diff? output/ErrorEffectSigDefDiff.expected
     output/ErrorEffectSigDefDiff.out)
    (diff? output/ErrorEffectsNoValueBranch.expected
     output/ErrorEffectsNoValueBranch.out)
    (diff? output/ErrorHangingVal.expected output/ErrorHangingVal.out)
    (diff? output/ErrorImportOfImport.expected
     output/ErrorImportOfImport.out)
    (diff? output/ErrorIncompatibleSig.expected
     output/ErrorIncompatibleSig.out)
    (diff? output/ErrorInconsistentValType.expected
     output/ErrorInconsistentValType.out)
    (diff? output/ErrorInexhaustivePattern.expected
     output/ErrorInexhaustivePattern.out)
    (diff? output/ErrorLetRecExpression.expected
     output/ErrorLetRecExpression.out)
    (diff? output/ErrorNameClashExtern.expected
     output/ErrorNameClashExtern.out)
    (diff? output/ErrorNameClashExternVal.expected
     output/ErrorNameClashExternVal.out)
    (diff? output/ErrorNameClashWithImport.expected
     output/ErrorNameClashWithImport.out)
    (diff? output/ErrorPartialApplication.expected
     output/ErrorPartialApplication.out)
    (diff? output/ErrorPatternUnionNameTypes.expected
     output/ErrorPatternUnionNameTypes.out)
    (diff? output/ErrorPolymorphicRecursion.expected
     output/ErrorPolymorphicRecursion.out)
    (diff? output/ErrorResumeEffects.expected output/ErrorResumeEffects.out)
    (diff? output/ErrorShadowedBool.expected output/ErrorShadowedBool.out)
    (diff? output/ErrorSignatureCompatibility.expected
     output/ErrorSignatureCompatibility.out)
    (diff? output/ErrorSubmoduleSignatures.expected
     output/ErrorSubmoduleSignatures.out)
    (diff? output/ErrorToplevelImport.expected
     output/ErrorToplevelImport.out)
    (diff? output/ErrorTypeDeclFreeParams.expected
     output/ErrorTypeDeclFreeParams.out)
    (diff? output/ErrorUnqualifiedUniversalImport.expected
     output/ErrorUnqualifiedUniversalImport.out)
    (diff? output/ExternInt.expected output/ExternInt.out)
    (diff? output/Functions.expected output/Functions.out)
    (diff? output/Generalization.expected output/Generalization.out)
    (diff? output/HelloWorld.expected output/HelloWorld.out)
    (diff? output/Imports.expected output/Imports.out)
    (diff? output/LetBindingGroups.expected output/LetBindingGroups.out)
    (diff? output/LetPattern.expected output/LetPattern.out)
    (diff? output/ModuleSig.expected output/ModuleSig.out)
    (diff? output/Modules.expected output/Modules.out)
    (diff? output/MoreEffectsInSig.expected output/MoreEffectsInSig.out)
    (diff? output/MutualRecursion.expected output/MutualRecursion.out)
    (diff? output/OpSectionLeft.expected output/OpSectionLeft.out)
    (diff? output/OpSectionRight.expected output/OpSectionRight.out)
    (diff? output/Operators.expected output/Operators.out)
    (diff? output/Option.expected output/Option.out)
    (diff? output/OverlappingImports.expected output/OverlappingImports.out)
    (diff? output/Paths.expected output/Paths.out)
    (diff? output/RecursiveEffectDecl.expected
     output/RecursiveEffectDecl.out)
    (diff? output/SelfReferentialImport.expected
     output/SelfReferentialImport.out)
    (diff? output/Text.expected output/Text.out)
    (diff? output/Traits.expected output/Traits.out)
    (diff? output/TypeChecking.expected output/TypeChecking.out)
    (diff? output/TypeModuleNamePunning.expected
     output/TypeModuleNamePunning.out)
    (diff? output/Types.expected output/Types.out)
    (diff? output/WeirdFunCalls.expected output/WeirdFunCalls.out)))))
