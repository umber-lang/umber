include Umber_common

include struct
  open Umber_syntax
  include Names
  module Literal = Literal
end

include struct
  open Umber_mir
  module Block_index = Block_index
  module Cnstr_tag = Cnstr_tag
  module Effect_op_id = Effect_op_id
  module Mir = Mir
end
