;;; -*- Package: C -*-
(in-package 'c)

(use-package "PROFILE")

(profile ir1-top-level
	 find-initial-dfo
	 find-dfo
	 local-call-analyze
	 delete-block
	 join-successor-if-possible
	 ir1-optimize-block
	 flush-dead-code
	 generate-type-checks
	 constraint-propagate
	 pre-environment-analyze-top-level
	 environment-analyze
	 gtn-analyze
	 control-analyze
	 ltn-analyze
	 stack-analyze
	 ir2-convert
	 select-representations
	 lifetime-pre-pass
	 lifetime-flow-analysis
	 reset-current-conflict
	 lifetime-post-pass
	 delete-unreferenced-tns

	 pack-wired-tn
	 pack-tn
	 pack-targeting-tns
	 pack-load-tns
	 emit-saves

	 generate-code
	 fasl-dump-component
	 clear-ir2-info
	 macerate-ir1-component
	 merge-top-level-lambdas
	 check-free-function
	 note-failed-optimization
	 clear-stuff
	 read-source-form
	 fasl-dump-source-info
	 fasl-dump-top-level-lambda-call
;	 check-life-consistency
;	 check-ir1-consistency
;	 check-ir2-consistency
	 )
