[%%import: type a = Stuff.a]

module type Example = sig
  module type%import InnerModule = Stuff.S_optional
end
