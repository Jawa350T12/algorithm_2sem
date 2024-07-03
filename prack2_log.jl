############## 
##### Log #### 
############## 
 
function __log_vanilla(x::T, base::T=2, eps::AbstractFloat=1e-8)::T where {T} # base > 1 
    @assert(base > 1) 
    @assert(x > 0) 
    z = x 
    t = 1.0 
    y = 0.0 
    while z < 1 / base z > base t > eps 
        if z < 1 / base 
            z *= base 
            y -= t 
        elseif z > base 
            z /= base 
            y += t 
        elseif t > eps 
            t /= 2 
            z *= z 
        end 
    end 
    return y 
end 
 
function log_iterative(x::T, base::T=2, eps::AbstractFloat=1e-8)::T where {T} 
    @assert(base > 1 || base < 1) 
    @assert(x > 0) 
 
    if base > 1 
        return __log_vanilla(x, base, eps) 
    else 
        return -__log_vanilla(x, 1 / base, eps) 
    end 
end 
 
function root(p::Polynomial{Complex{T}}, x0::Complex{T}, eps::AbstractFloat=1e-8)::Union{Complex{T},Nothing} where {T} 
    res = newton_classic(x -> p(x), x -> valdiff(p, x)[2], x0, eps) 
    if abs(p(res)) > eps 
        return nothing 
    end 
    return res 
end 
 
function tailor_cos_fixprec(x::T, n::Integer)::T where {T} 
    res = zero(typeof(x)) 
    ak = 1 
    for k in 1:n 
        coeff = -(x * x) / (2 * k * (2 * k - 1)) 
        ak = ak * coeff 
        res += ak 
    end 
    return res 
end 
 
function tailor_cos(x::T)::T where {T} 
    res = zero(typeof(x)) 
    ak = 1 
    k = 1 
    while abs(ak) > Base.eps(typeof(x)) 
        res += ak 
        coeff = -(x * x) / (2 * k * (2 * k - 1)) 
        ak = ak * coeff 
        k += 1 
    end 
    return res 
end 
 
function tailor_e(x::T)::T

where {T} 
    res = zero(typeof(x)) 
    ak = 1 
    k = 1 
    while abs(ak) > Base.eps(typeof(x)) 
        res += ak 
        coeff = x / k 
        ak = ak * coeff 
        k += 1 
    end 
    return res 
end 
 
function tailor_bessel(x::T, a::Integer=0)::T where {T} 
    res = zero(T) 
    ak = (x / 2.0)^a / float(factorial(a)) 
    k = 1 
    while abs(ak) > eps(T) 
        res += ak 
        coeff = -(x * x) / float(4 * k * (k + a)) 
        ak = ak * coeff 
        k += 1 
    end 
    return res 
end 
 
function jacobian_matrix(system::Vector{Function}, argument_vector::Vector{T})::Matrix{T} where {T} 
    @assert(length(system) == length(argument_vector)) 
 
    n = length(system) 
    res = zeros(T, n, n) 
 
    for i in 1:n 
        func = system[i] 
        for j in 1:n 
            arguments = Vector{Union{Dual{T},T}}(argument_vector) 
            arguments[j] = Dual{T}(arguments[j]) 
            part_der = imag(func(arguments...)) 
            res[i, j] = part_der 
        end 
    end 
 
    return res 
end 
 
 
# Backcompat on matrix 
function jacobian_matrix(system::Vector{Function}, argument_vector::Matrix{T})::Matrix{T} where {T} 
    @assert(length(system) == length(argument_vector)) 
 
    n = length(system) 
    res = zeros(T, n, n) 
 
    for i in 1:n 
        func = system[i] 
        for j in 1:n 
            arguments = Matrix{Union{Dual{T},T}}(argument_vector) 
            arguments[j, 1] = Dual{T}(arguments[j]) 
            part_der = imag(func(arguments...)) 
            res[i, j] = part_der 
        end 
    end 
 
    return res 
end 
 
function calc_system(system::Vector{Function}, args::Matrix{T})::Matrix{T} where {T} 
    res = zeros(T, length(system), 1) 
    for i in eachindex(system) 
        resi] = system[i 
    end 
    return res 
end
