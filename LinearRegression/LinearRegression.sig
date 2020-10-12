signature LinearRegression =
sig
  structure Seq : SEQUENCE

  type 'a seq = 'a Seq.t
  type lr

  exception Range

  val fit : real seq seq -> real seq -> lr
  val predict : lr -> real seq -> real seq

end
