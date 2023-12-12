type regexp
(** The type of regular expressions *)

type split_result = Str.split_result =
  | Text of string
  | Delim of string  (** Here we keep compatibility with [Str] *)

type result
(** The type of matching results *)

val regexp : string -> regexp
(** Parses a regexp *)

val search_forward : regexp -> string -> int -> int * result
(** Searches a match of the string with the regexp, starting at
   * the position and in forward direction.
   * Raises [Not_found] if no match could be found.
   * Returns [(p,r)] when a match at position [p] is found,
   * described by [r].
   *)

val quote_set : string -> string
(** Returns a regexp (as string) that matches any of the characters in
      the argument. The argument must be non-empty
   *)

val matched_string : result -> string -> string
(** Extracts the matched part from the string. The string argument
  * must be the same string passed to [string_match] or the search
  * functions, and the result argument must be the corresponding
  * result.
  *)

val match_beginning : result -> int
(** Returns the position where the matched part begins *)

val match_end : result -> int
(** Returns the position where the matched part ends *)

val matched_group : result -> int -> string -> string
(** Extracts the substring the nth group matches from the whole
   * string. The string argument
   * must be the same string passed to [string_match] or the search
   * functions, and the result argument must be the corresponding
   * result.
   *)

val full_split : regexp -> string -> split_result list
(** Like [split_delim], but returns the delimiters in the result *)

val global_substitute :
  regexp -> (result -> string -> string) -> string -> string
(** [global_substitute re subst s]: Applies the substitution function
  * [subst] to all matchings of [re] in [s], and returns the 
  * transformed string. [subst] is called with the current [result]
  * of the match and the whole string [s].
  *)
