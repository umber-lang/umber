include Umber_common

include struct
  open Umber_syntax
  module Literal = Literal
  module Names = Names
end

include struct
  open Umber_mir
  module Cnstr_tag = Cnstr_tag
  module Mir = Mir
end
