using Base: gcd, lcm 
 
 
struct Residue{T,M} 
    value::T 
 
    function Residue{T,M}(value::T) where {T,M} 
        return new(Base.mod(value, M)) 
    end 
end 
 
ringmod(::Residue{T,M}) where {T,M} = M 
value(a::Residue{T,M}) where {T,M} = a.value 
Base.zero(::Type{Residue{T,M}}) where {T,M} = Residue{T,M}(Base.zero(T)) 
Base.zero(::Residue{T,M}) where {T,M} = Residue{T,M}(Base.zero(T)) 
Base.one(::Type{Residue{T,M}}) where {T,M} = Residue{T,M}(Base.one(T)) 
Base.one(::Residue{T,M}) where {T,M} = Residue{T,M}(Base.one(T)) 
Base.eps(::Type{Residue{T,M}}) where {T,M} = Residue{T,M}(Base.eps(T)) 
Base.eps(::Residue{T,M}) where {T,M} = Residue{T,M}(Base.eps(T)) 
 
Base.:(+)(a::Residue{T,M}, b::Residue{T,M}) where {T,M} = Residue{T,M}(a.value + b.value) 
Base.:(-)(a::Residue{T,M}, b::Residue{T,M}) where {T,M} = Residue{T,M}(a.value - b.value) 
Base.:(-)(a::Residue{T,M}) where {T,M} = Residue{T,M}(-a.value) 
Base.:(*)(a::Residue{T,M}, b::Residue{T,M}) where {T,M} = Residue{T,M}(a.value * b.value) 
Base.:(^)(a::Residue{T,M}, b::Residue{T,M}) where {T,M} = Residue{T,M}(a.value^b.value) 
Base.:(==)(r1::Residue{T, M}, r2::Residue{T, M}) where {T, M} = r1.value == r2.value 
 
function Base.invmod(a::Residue{T,M})::Union{Residue{T,M},Nothing} where {T,M} 
    gcd, inverse, _ = gcdx_(a.value, M) 
    if gcd != one(a.value) 
        return nothing 
    end 
    return Residue{T,M}(mod(inverse, M)) 
end 
 
Base.:(/)(a::Residue{T,M}, b::Residue{T,M}) where {T,M} = a * invmod(b) 
 
function Base.display(r::Residue{T,M}) where {T,M} 
    println("$(r.value) (mod $M)") 
end 
 
 
function gcdx_(a::T, b::T)::Tuple{T,T,T} where {T} 
    gcd = a 
    x = one(T) 
    y = zero(T) 
    x_prev = y 
    y_prev = x 
 
    while !iszero(b) 
        quotient, remainder = divrem(gcd, b) 
        gcd, b = b, remainder 
 
        x, x_prev = x_prev, x - quotient * x_prev 
        y, y_prev = y_prev, y - quotient * y_prev 
    end 
 
    if gcd < 0 
        gcd, x, y = -gcd, -x, -y 
    end 
 
    return gcd, x, y 
end 
 
function diaphant_solve(a::T, b::T, c::T)::Union{Tuple{T,T},Nothing} where {T} 
    if mod(a, c) != 0 || mod(b, c) != 0 
        return nothing 
    end 
    a /= c 
    b /= c 
    _, x, y = gcdx_(a, b) 
    return x, y 
end 
 
 
function
