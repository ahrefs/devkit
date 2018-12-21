
module Devkit_core : sig
  include module type of Devkit_core with module Test := Devkit_core.Test
end = struct
  include Devkit_core
end

include Devkit_core
include Prelude
