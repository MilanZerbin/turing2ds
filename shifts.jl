# Data Structure for BiInfiniteSequences. Not actually Infinite.
struct BiInfSeq
    vision::Int # how much can be seen by f and g
    lrange::Int # how long is the left array
    rrange::Int # how long is the right array
    l::Vector{Bool}(False, lrange) # left array
    r::Vector{Bool}(False, rrange) # right array
end

# helper function that shifts the decimal point by 1 to the right
function shiftright(B::BiInfSeq)
    return BiInfSeq(B.vision, B.lrange, B.rrange, ...)

# helper function that shifts the decimal point by "k", allowing negative "k".
function shiftbyk(B::BiInfSeq, k::Int)


# performs one iteration of the DS
function step(B::BiInfSeq, f, g)
    # look only at allowed "window" of the array
    lwindow = @view B.l[1:B.vision]
    rwindow = @view B.r[1:B.vision]
    steppedB = BiInfSeq()
