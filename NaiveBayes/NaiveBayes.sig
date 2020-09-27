signature NB =
sig
  structure Seq : SEQUENCE

  type 'a seq = 'a Seq.t
  datatype feature = Disc of int | Cont
  type nb

  exception Range

  val fit : feature seq * int -> int seq seq -> int seq -> nb
  val predict : nb -> int seq -> int seq
  val test : nb -> int seq -> int seq -> real seq * real
  val prior : nb -> real seq
  val param : nb -> int -> int -> int -> real

end
