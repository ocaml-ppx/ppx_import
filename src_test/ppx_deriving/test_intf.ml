[%%import: type a = Stuff.a]

module type Example = sig
  module type InnerModule = [%import: (module Stuff.S_optional)]
end
