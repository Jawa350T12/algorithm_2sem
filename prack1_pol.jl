abstract type AbstractPolynomial <: Number end 
 
struct Polynomial{T} <: AbstractPolynomial 
    coeffs::Vector{T} 
 
    function Polynomial{T}() where {T} 
        return new{T}(Vector{T}([Base.zero(T)])) 
    end 
 
    function Polynomial{T}(coeff::Vector{T}) where {T} 
        if length(coeff) < 1 
            return new{T}(Vector{T}([Base.zero(T)])) 
        end 
 
        index = findlast(x -> !Base.iszero(x), coeff) 
        if isnothing(index) 
            return new{T}(Vector{T}([Base.zero(T)])) 
        end 
 
        return new{T}(coeff[begin:index]) 
    end 
 
    function Polynomial{T}(coeff::Tuple) where {T} 
        if length(coeff) < 1 
            return new{T}(Vector{T}([Base.zero(T)])) 
        end 
 
        index = findlast(x -> !Base.iszero(x), coeff) 
        if isnothing(index) 
            return new{T}(Vector{T}([Base.zero(T)])) 
        end 
 
        return new{T}(Vector{T}([coeff[begin:index]...])) 
    end 
 
    function Polynomial{T}(::Type{T}) where {T} 
        return new{T}(Vector{T}([Base.zero(T)])) 
    end 
 
    function Polynomial{T}(coeff::T) where {T} 
        return new{T}(Vector{T}([coeff])) 
    end 
end 
     
 
 
#  
#  
#  BASIC ACCESS 
#  
#  
 
function lead(p::Polynomial{T})::T where {T} 
    return last(p.coeffs) 
end 
 
function value(p::Polynomial{T}, arg::T)::T where {T} 
    res = zero(T) 
    for i in eachindex(p.coeffs) 
        res += arg^(i - 1) * T(p.coeffs[i]) 
    end 
    return res 
end 
 
function ord(p::Polynomial{T})::Integer where {T} 
    return length(p.coeffs) - 1 
end 
 
function evaluate(p::Polynomial{T}, x::T) where {T <: Number} 
    sum = zero(T) 
    for coeffs in p.coeffss 
        sum += coeffs * x^length(p.coeffss) 
        x *= x 
    end 
    return sum 
end 
 
function coeffs(p::Polynomial{T}) where {T} 
    return Base.copy(p.coeffs) 
end 
 
function coeff_tuple(p::Polynomial{T})::NTuple{length(p.coeffs),T} where {T} 
    return Tuple(p.coeffs) 
end 
 
degree(p::Polynomial) = length(p.coeffss) - 1 
 
function copy(p::Polynomial{T})::Polynomial{T} where {T} 
    r = Base.copy(p.coeffs) 
    return Polynomial{T}(r) 
end 
 
function Base.zero(::Type{Polynomial{T}})::Polynomial{T} where {T} 
    return Polynomial{T}(Base.zero(T)) 
end 
 
function Base.zero(::Polynomial{T})::Polynomial{T} where {T} 
    return Polynomial{T}(Base.zero(T)) 
end 
 
function Base.one(::Polynomial{T})::Polynomial{T} where {T} 
    return Polynomial{T}(Base.one(T)) 
end 
 
function Base.one(::Type{Polynomial{T}})::Polynomial{T} where {T} 
    return Polynomial{T}(Base.one(T)) 
end 
 
#  
#  
# OPERATORS SUBFUNCTIONS 
#  
#  
 
function Base.divrem(p::Polynomial{T}, q::Polynomial{T})::Tuple{Polynomial{T},Polynomial{T}} where {T} 
    if q == Polynomial{T}() 
        throw(ErrorException("polynomial division by 0-polynomial")) 
    end 
 
    quotent = Polynomial{T}() 
    remainder = copy(p) 
 
    while remainder != Polynomial{T}() && ord(remainder) >= ord(q) 
        coeffs = Vector{T}() 
        power = ord(remainder) - ord(q) 
        for _ in 1:power 
            append!(coeffs, [Base.zero(T)]) 
        end 
        append!(coeffs, [lead(remainder) / lead(q)]) 
        t = Polynomial{T}(coeffs) 
 
        quotent = quotent + t 
        remainder = remainder - t * q 
    end 
 
    return (quotent, remainder) 
end 
 
function Base.mod(p::Polynomial{T}, q::Polynomial{T})::Polynomial{T} where {T} 
    return (divrem(p, q))[2] 
end 
function Base.mod(p::Polynomial{T}, q::Tuple)::Polynomial{T} where {T} 
    return (divrem(p, Polynomial{T}(q)))[2] 
end 
 
function Base.:<(p::Polynomial{T}, x::T) where {T} 
    return all(c -> c < x, p.coeffs) 
end 
 
function Base.:<(x::T, p::Polynomial{T}) where {T} 
    return all(c -> x < c, p.coeffs) 
end 
 
#  
#  
#  OPERATORS 
#  
# 
 
# ADDITION 
 
function Base.:(+)(q::Polynomial{T}, p::Polynomial{T})::Polynomial{T} where {T} 
    n = max(length(q), length(p)) 
    c_q = vcat(coeffs(q), zeros(T, n - length(q))) 
    c_p = vcat(coeffs(p), zeros(T, n - length(p))) 
    return Polynomial{T}(c_q .+ c_p) 
end 
 
function

function Base.:(+)(a::Vector{T}, b::T) where T 
    new_coeffs = vcat([a[1] + T(b)],a[2:end-1]) 
    return new_coeffs 
end 
 
function Base.:(+)(a::Polynomial{T}, b::T) where T 
    new_coeffs = vcat([a.coeffs[1] + T(b)],a.coeffs[2:end]) 
    return Polynomial{T}(new_coeffs) 
end 
 
Base.:(+)(a::T, b::Polynomial{T}) where T =  b+a 
 
# SUBSTRACTION 
 
function Base.:(-)(arr::Polynomial{T})::Polynomial{T} where T 
    a = copy(arr.coeffs) 
    return Polynomial{T}(.-a.coeffs) 
end 
 
function Base.:(-)(q::Polynomial{T}, p::Polynomial{T})::Polynomial{T} where {T} 
    n = max(length(q), length(p)) 
    c_q = vcat(coeffs(q), zeros(T, n - length(q))) 
    c_p = vcat(coeffs(p), zeros(T, n - length(p))) 
    return Polynomial{T}(c_q .- c_p) 
end 
 
function Base.:(-)(a::Polynomial{T}, b::T) where T 
    new_coeffs = vcat([a.coeffs[1] + T(b)],a.coeffs[2:end]) 
    return Polynomial{T}(new_coeffs) 
end 
 
Base.:(-)(a::T, b::Polynomial{T}) where T =  -b+a 
 
# EQUALITY 
 
function Base.:(==)(arr::Polynomial{T}, arr2::Polynomial{T})::Bool where T 
    return arr.coeffs == arr2.coeffs 
end 
 
function Base.:(!=)(arr::Polynomial{T}, arr2::Polynomial{T})::Bool where T 
    return arr.coeffs != arr2.coeffs 
end 
 
# MULTIPLICATION 
 
function Base.:(*)(p::Polynomial{T}, q::Polynomial{T})::Polynomial{T} where {T} 
    r = Polynomial{T}(Vector{T}()) 
    for i in eachindex(p.coeffs) 
        temp_coeffs = Vector{T}() 
        for _ in 1:(i-1) 
            append!(temp_coeffs, [Base.zero(T)]) 
        end 
        for j in eachindex(q.coeffs) 
            append!(temp_coeffs, [p.coeffs[i] * q.coeffs[j]]) 
        end 
        r = r + Polynomial{T}(temp_coeffs) 
    end 
    return r 
end 
 
function Base.:(*)(p::Polynomial{T}, a::T)::Polynomial{T} where {T} 
    return Polynomial{T}(p.coeffs .* a) 
end 
 
function Base.:(*)(a::T, p::Polynomial{T})::Polynomial{T} where {T} 
    return Polynomial{T}(p.coeffs .* a) 
end 
 
# DIVISION 
 
function Base.:(//)(p::Polynomial{T}, q::Polynomial{T})::Polynomial{T} where {T} 
    return (divrem(p, q))[1] 
end 
 
function Base.:(/)(p::Polynomial{T}, q::Polynomial{T})::Tuple{Polynomial{T},Polynomial{T}} where {T} 
    return Base.divrem(p, q) 
end 
 
function Base.:(÷)(p::Polynomial{T}, q::Polynomial{T})::Polynomial{T} where {T} 
    return (divrem(p, q))[1] 
end 
 
function Base.:(%)(p::Polynomial{T}, q::Polynomial{T})::Polynomial{T} where {T} 
    return (divrem(p, q))[2] 
end 
 
# BASE OVERLOAD 
 
function Base.length(p::Polynomial{T}) where {T} 
    return size(p.coeffs)[1] 
end 
 
function Base.copy(p::Polynomial{T})::Polynomial{T} where {T} 
    r = Base.copy(p.coeffs) 
    return Polynomial{T}(r) 
end 
 
# EVAL AT THE GIVEN POINT 
 
# function valdiff(p::Polynomial{T}, x::T) where {T} 
#     res,d_res = zero(T),zero(T) 
#     p_c=copy(p.coeffs) 
#     n = length(p) 
 
#     for i in n:-1:2 
#         res = p_c[i] + (x * res) 
#         d_res = (i-1) * p_c[i] + (x * d_res) 
#     end 
 
#     res = p_c[1] + (x * res) 
#     return res, d_res 
# end 
 
function valdiff(p::Polynomial{T}, x::T) where {T} 
    res, diffres = Base.zero(T), Base.zero(T) 
    for i in lastindex(p.coeffs):-1:2 
        res = res*x + p.coeffs[i] 
        diffres = diffres*x + (i-1)*p.coeffs[i] 
    end 
    res = res*x + p.coeffs[1] 
 
    return (res, diffres) 
end 
 
function (p::Polynomial{T})(x) where {T} 
    if length(p.coeffs) == 1 
        return p.coeffs[1] 
    end 
 
    res = 0 
    for i in lastindex(p.coeffs):-1:1 
        res = res * x + p.coeffs[i] 
    end 
 
    return res 
end 
 
function Base.display(p::Polynomial{T})::String where {T} 
    res = "" 
    for i in reverse(eachindex(p.coeffs)) 
        if p.coeffs[i] != zero(T) 
            if i == length(p.coeffs) 
                res *= "$(p.coeffs[i])x^$(i-1)" 
            elseif i == 2 
                res *= " + $(p.coeffs[i])x" 
            elseif i == 1 
                res *= " + $(p.coeffs[i])" 
            else 
                res *= " + $(p.coeffs[i])x^$(i-1)" 
            end 
        end 
    end 
    println(res) 
    return res 
end

# DEBUG 
 
function pol(v::Vector{T}) where {T} 
    return Polynomial{T}(v) 
end 
