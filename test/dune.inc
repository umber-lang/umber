(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/basics.expected tokens/basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/basics.expected ast/basics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/classics.expected tokens/classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/classics.expected ast/classics.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/empty.expected tokens/empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/empty.expected ast/empty.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/errorDuplicateLet.expected tokens/errorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/errorDuplicateLet.expected ast/errorDuplicateLet.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/errorDuplicateVal.expected tokens/errorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/errorDuplicateVal.expected ast/errorDuplicateVal.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/functions.expected tokens/functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/functions.expected ast/functions.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/imports.expected tokens/imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/imports.expected ast/imports.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/letPattern.expected tokens/letPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/letPattern.expected ast/letPattern.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/modules.expected tokens/modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/modules.expected ast/modules.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer
   (diff tokens/mutualRecursion.expected tokens/mutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff ast/mutualRecursion.expected ast/mutualRecursion.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/operators.expected tokens/operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/operators.expected ast/operators.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/option.expected tokens/option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/option.expected ast/option.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/text.expected tokens/text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/text.expected ast/text.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/traits.expected tokens/traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/traits.expected ast/traits.out))))

(rule (alias runtest) (deps test.dummy)
 (action
  (no-infer (diff tokens/typeChecking.expected tokens/typeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/typeChecking.expected ast/typeChecking.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff tokens/types.expected tokens/types.out))))

(rule (alias runtest) (deps test.dummy)
 (action (no-infer (diff ast/types.expected ast/types.out))))

