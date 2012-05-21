(* #####################################################
 * Helpers
 * ##################################################### *)

(* assert helpers from http://bit.ly/JJXRN6 *)
exception Assert of string;

val assert : bool * string -> unit =
  fn  (false, s)  => raise (Assert s)
  |   (true, _)   => ignore 0;

(* returns true when list contains element x with f(x) = true *)
fun find(f, nil)   = false
|   find(f, x::xs) = if f(x) then true else find(f, xs); 


(* ####################################################
 * Exercise 2
 * #################################################### *)

(* observe the following datatype definition.. *)
datatype dirtree = file of string | dir of string * dirtree list;

(* ..and the following values of this type. *)
val d1 = dir("Holiday-2010", [file("pic1.jpg"), file("pic2.jpg")]);
val d2 = dir("Holiday-2011", [file("a.jpg"), file("b.jpg"), file("c.jpg")]);
val d3 = dir("Holidays", [file("sun.jpg"), d1, d2, file("me.jpg")]);


(* a) write a function filecount which counts the # of files in a dirtree *)

fun filecount(file(_))        = 1
|   filecount(dir(_, nil))    = 0
|   filecount(dir(dirname, x::xs))  =
      filecount(x) + filecount(dir(dirname, xs));

assert(filecount d3 = 7,
  "#filecount returns the corrent number of files in the given dirtree");

assert(filecount (file("foo.bar")) = 1,
  "#filecount returns 1 if the given dirtree is a file");

(* b) write a function absfilelist which prints absolute file paths for all files *)

local
  fun absfilelist_tree(nil, prefix)                       = nil
  |   absfilelist_tree(dir(dirname, content)::xs, prefix) =
        absfilelist_tree(content, prefix ^ "/" ^ dirname) @
          absfilelist_tree(xs, prefix)
  |   absfilelist_tree(file(filename)::xs, prefix)        =
        prefix ^ "/" ^ filename :: absfilelist_tree(xs, prefix)
in
  fun absfilelist(dir(dirname, content))  = absfilelist_tree(content, "/" ^ dirname)
  |   absfilelist(file(filename))         = ["/" ^ filename]
end;


assert(length (absfilelist d3) = 7,
  "#absfilelist returns a list with the corrent number of paths");
