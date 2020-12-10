"onion_logic" <- function(e1,e2){  # also used for onionmat logic
  stop("No logic currently implemented for onions")
}

setMethod("Logic",signature(e1="onion",e2="onion"  ), onion_logic)
setMethod("Logic",signature(e1="ANY"  ,e2="onion"  ), onion_logic)
setMethod("Logic",signature(e1="onion",e2="ANY"    ), onion_logic)
setMethod("Logic",signature(e1="onion",e2="missing"), onion_logic)

setMethod("Logic",signature(e1="onionmat",e2="onionmat"  ), onion_logic)
setMethod("Logic",signature(e1="ANY"  ,e2="onionmat"  ), onion_logic)
setMethod("Logic",signature(e1="onionmat",e2="ANY"    ), onion_logic)
setMethod("Logic",signature(e1="onionmat",e2="missing"), onion_logic)

setMethod("Logic",signature(e1="onionmat",e2="onion"  ), onion_logic)
setMethod("Logic",signature(e1="onion",e2="onionmat"  ), onion_logic)

