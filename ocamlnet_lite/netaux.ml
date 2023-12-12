module ArrayAux = struct
  let int_blit_ref =
    ref 
      (fun (src:int array) srcpos dest destpos len ->
	 (* A specialised version of Array.blit for int arrays.
	  * Faster than the polymorphic Array.blit for
	  * various reasons.
	  *)
	 if (len < 0 || srcpos < 0 || 
	     srcpos+len > Array.length src ||
	     destpos < 0 ||
	     destpos+len > Array.length dest) then
	   invalid_arg "Netaux.ArrayAux.int_blit";
	 if src != dest || destpos <= srcpos then (
	   for i = 0 to len-1 do
	     Array.unsafe_set 
	       dest 
	       (destpos+i) 
	       (Array.unsafe_get src (srcpos+i))
	   done
	 ) else (
	   for i = len-1 downto 0 do
	     Array.unsafe_set 
	       dest 
	       (destpos+i) 
	       (Array.unsafe_get src (srcpos+i))
	   done
	 )
      )

  let int_blit src srcpos dest destpos len = 
    !int_blit_ref src srcpos dest destpos len

end
