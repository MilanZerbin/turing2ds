using DynamicalSystems
using CairoMakie

# Rotates unit disk by angle "theta" 
function rotationmap(u0=zeros(2); theta = 1.0)
    rot_eom(x, p) = SVector{2}(cos(p[1])x[1] - sin(p[1])x[2], cos(p[1])x[2] + sin(p[1])x[1])
    rot_jacob(x, p) = @SMatrix [0.0 -1.0;1.0 0.0]
    return DiscreteDS(u0, rot__eom, rot_jacob; parameters = [theta])
end

# applies the (disk preserving) moebius transform e^{i\theta}(z +  a + ib)/(1 + z(a-ib)) to z = x.
function moebius(u_0=zeros(2); a = 1.0, b = -1.0, theta = 1.0)
    reM = x[1] + p[1] + p[1](x[1]^2 + x[2]^2) + (p[1]^2 - p[2]^2)x[1] - 2p[1]p[2]x[2]
    imM = x[2] + p[2] + p[2](x[1]^2 + x[2]^2) + (p[1]^2 - p[2]^2)x[2] + 2p[1]p[2]x[1]
    denum = 1/ ((1 + p[1]x[1] + p[2]x[2])^2 + (x[2]p[1] - x[1]p[2])^2)
    moebius_eom(x, p) = SVector{2}(cos(p[3])reM - sin(p[3])imM, cos(p[3])imM + sin(p[3])reM)
    return DiscreteDS(u0, moebius_eom; parameters = [a, b, theta])
end

function f(initialu)
    return evolve(moebius, 1 [,initialu])

fig, ax, pl = streamplot(f, -1.0..1.0, -1.0..1.0, colormap = :magma)
