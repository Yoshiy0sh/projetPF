module type Game =
  sig
  val dt : float
  val box_x : float * float
  val box_y : float * float

  (* Accélération *)
  val g : float

end

module type Coll =
  sig
  

end



let game_hello () = print_endline "Hello, Newtonoiders!"
