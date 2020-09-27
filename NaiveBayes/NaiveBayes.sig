signature NB =
sig
  structure Seq : SEQUENCE

  type nb
  type 'a seq = 'a Seq.t

  exception Range

  val fit : int seq seq -> int seq seq -> int seq -> nb
  val predict : nb -> int seq -> int seq
  val test : nb -> int seq -> int seq -> real seq * real
  val prior : nb -> real seq
  val param : nb -> int -> int -> int -> real

end
