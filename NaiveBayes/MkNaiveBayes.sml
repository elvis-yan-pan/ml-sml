functor NaiveBayes (
  structure Seq: SEQUENCE
) :> NB where 'a seq = 'a Seq.t =
struct

  type 'a seq = 'a Seq.t

  exception Unimplemented

  fun fit range nlabels features labels = raise Unimplemented

end
