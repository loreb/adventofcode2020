(* Skipped yesterday because real life got in the way... *)
(* polyc needs libpolyml-dev;
* can't use command line args from the interpreter?
* it interprets ALL args as  source files!
*)

(* XXX I'm such an idiot, SML is ALLOWED to raise on overflow!
* SML/NJ does that at 1073741823 aka 0x3fffffff
* polyml does that at 4611686018427387903 aka 0x3fffffffffffffff
* I guess they use tagging or something
*)

(*
* """
* 939
* 7,13,x,x,59,x,31,19
* """
* means: every seven minutes, every 13, unavailable, unavailable, every 59, ...
* must depart as soon as possible but not before t=939
* => 944%59 = 0, so the 1st bus that passes at a time >= 939 is bus 59
*)

exception crap of string

fun println s = ( print s; print "\n" )
fun atoi s =
  case Int.fromString s of
       NONE => raise crap s
     | SOME n => n

fun printn n = print(Int.toString n)

fun join xs separator = (* as in perl *)
  foldr (fn(x,y) => x ^ separator ^ y) "" xs
fun ints2s intlist =
  join (map Int.toString intlist) " "

fun readlines fd =
  case TextIO.inputLine fd of
       SOME line => line :: readlines(fd)
     | NONE      => []

(* "7,13,x,x,59,x,31,19\n" => [7,13,NONE,NONE,59,NONE,31,19] *)
fun parseIDs line = (* XXX  modified! *)
let
  (* splitting uses "substrings", not plain "strings" (ie slices) *)
  (* XXX split is weird, so just use the "fields" or "tokens" fn *)
  fun extractIDs fields =
    case fields of
         [] => []
       | "x"::rest => NONE :: (extractIDs rest)
       | id::rest => SOME(atoi(id)) :: (extractIDs rest)
  fun iscomma ch = ch = #","
  val substrings = Substring.fields iscomma (Substring.full line)
in
  extractIDs(map Substring.string substrings)
end

(* I woke up too early to figure this out at nearly midnight... *)
fun tired_delay(departure, period) =
  if false
  then period - (departure mod period)
  else
    if departure mod period = 0
    then 0
    else period * (departure div period + 1) - departure
fun delay(departure, period) =
  (period - (departure mod period)) mod period (* handle zero *)

fun gcd(a,b) =
    if b = 0
    then a
    else gcd(b, a mod b)
fun lcm(a,b) =
    (a*b) div gcd(a,b)

fun id2string id =
  case id of
       NONE => "x"
     | SOME(n) => Int.toString(n)

fun process2(ids) =
  (* FUCK! the thing that got me last night is that I assumed period > delay,
  * while in the test input there's eg a bus that passes every 13 minutes
  * in position 36 - OF COURSE it can't be 36 minutes late,
  * but it CAN be 36 modulo 13 minutes late :D
  *)
  let
    (* t is ok if t%a[0] == 0, t%a[1]==1 etc; a[n]=0 => ok *)
    fun ok(t, left, wanteddelay) =
      case left of
           [] => true
         | NONE::rest => ok(t, rest, wanteddelay+1)
         (*| x::rest => t mod x = wantedmodulo andalso ok(t, rest,
         * wantedmodulo+1) -- I'm such an idiot at times... *)
         | SOME(x)::rest => delay(t,x) = wanteddelay andalso ok(t, rest, wanteddelay+1)
    fun first_multiple_with_given_delay(candidate, step, period, d) =
    let val _ = println("m1: " ^ (ints2s [candidate, step, period, d]))
    in
      if d >= period
      then first_multiple_with_given_delay(candidate, step, period, d mod
      period)
      else
        if delay(candidate, period) = d
        then candidate
        else (
        println("delay is " ^ (Int.toString(delay(candidate,period))));
        first_multiple_with_given_delay(candidate+step, step, period, d)
        )
    end
    (*
    * Yesterday I was so tired I couldn't reason correctly,
    * so I went to bed and... I kept thinking about this instead of sleeping :D
    * Anyway, the idea is easy:
    * candidate = 0
    * delta = 1
    * 1st => trivially ok; delta = mcm(delta, a[0])
    * 2nd => candidate += delta until delay(candidate, a[1]) = 1
    *        then delta = mcm(delta, a[1])
    *        so that every time we increase candidate by delta
    *        it stays candidate%a[0] = 0 and candidate%a[1] = 1
    * 3rd => ...
    *)
    fun find_t(candidate, delta, NONE::rest, index) =
      find_t(candidate, delta, rest, index+1)
      | find_t(candidate, delta, [], index) = candidate
      | find_t(candidate, delta, SOME(id)::rest, index) =
      let
        val newcandidate = first_multiple_with_given_delay(candidate, delta, id,
      index)
        val newdelta = lcm(delta, id)
        val _ = println("find_t: " ^ (ints2s [candidate, delta, id, index]))
      in
        find_t(newcandidate, newdelta, rest, index+1)
      end
  in
    (
    println(join (map id2string ids) " ");
    printn(find_t(0, 1, ids, 0))
    )
  end


fun part2 filename =
  let
    val qwerty = println("file=" ^ filename)
    val input = TextIO.openIn(filename)
    val lines = readlines(input)
    val ignored = hd(lines)
    val secondline = hd(tl(lines))
  in
    (
    TextIO.closeIn(input) ;
    process2(parseIDs(secondline))
    )
  end


fun doit args =
  case args of
       [] => 42
     | arg::rest => ( part2(arg); doit rest )
fun main() = ( doit(CommandLine.arguments()); print "\nOK\n" ;
  OS.Process.exit(OS.Process.success) )

(* val _ = main() polyml *)
