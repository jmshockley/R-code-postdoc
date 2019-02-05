R1=Inf;        #Flat surface
E1= 1.8E+11;          #Elastic modulus of flat surface in N/m^2 (Pa)
v1= .32;          #Poisson Ratio of flat surface

R2= 0.003175;    #Radius of ball/hemispehre in m
#R2=0.00000015 #AFM 150 nm tip radius
#E2= 64000000000; #nylon  #Elastic modulus of ball/hemisphere in N/m^2 (Pa)
#E2=1050000000000 #diamond
#E2=67000000000; #Pyrex
E2=393000000000 #Al2O3
v2= 0.2;        #Al2O3 #Poisson Ratio of ball/hemisphere
#v2=0.27         #diamond
Fn= 1;          #Normal load (N)
#Fn=10*41.56*112.2*10^-9 #10V assuming 41.56 N/m spring constant, 112.2 deflection sensitivity, 10V setpoint
mu= 0;          #Coefficient of Friction 
Ft= mu*Fn ;    #Tangential load
Note=toString("Al2O3_3.175mm_vs_k500")

#COMPUTATION OF HERTZIAN STRESS
R=1/(1/R1+1/R2);                    #Reduced Radius
E=1/((1-v1^2)/E1 + (1-v2^2)/E2);    #Reduced Modulus
a= ((3*Fn*R2)/(4*E))^(1/3);         #Contact half-width (m)
#xvalues= seq(from=-2*a, by=.004*3*a, to=2*a);               #Discrete values of x-axis (m)
#zvalues= seq(from=0, by=0.002*3*a, to=2*a);                 #Discrete values of z-axis (m)

xvalues= seq(from=-0.000100, by=0.0000020, to=0.000100+0.0000020);               #Discrete values of x-axis (m)
zvalues= seq(from=0, by=0.0000010, to=0.000100+0.0000010);                 #Discrete values of z-axis (m)

#x= -100*10^-6:1.01*10^-6:100*10^-6; #-100*10^-6:1.01*10^-6:100*10^-6;     %Standard size of x
#z= 0:1.01*10^-6:100*10^-6;              #Standard size of z
#x= -60*10^-6:0.05*10^-6:-55*10^-6;     #size of x for Holger
#z= 0.0505*10^-6:0.05*10^-6:5*10^-6;              #size of z for Holger
y=0; #%-100*10^-6;                %Value of y due to semi-infinite plane
Ac=3.1416*a^2;                      #%Hertzian Contact Area (m^2)
P=Fn/Ac;                            #%Average Hertzian Pressure (N/m^2)
Po=3*P/2;                           #%Maximum Hertzian Pressure (N/m^2)

X=rep(xvalues, length(xvalues))
Z=sort(rep(zvalues, length(zvalues)))
Stressdf=data.frame(X,Z)
Stressdf$X[which(Stressdf$X==a & Stressdf$Z==0)]=Stressdf$X[which(Stressdf$X==a & Stressdf$Z==0)]*1.00001
Stressdf$r2=Stressdf$X^2+y^2;
Stressdf$r=sqrt(Stressdf$X^2+y^2);
Stressdf$A=Stressdf$r2+Stressdf$Z^2-a^2;
Stressdf$S=sqrt(Stressdf$A^2+4*a^2*Stressdf$Z^2);
Stressdf$M=sqrt((Stressdf$S+Stressdf$A)/2);
Stressdf$N=sqrt((Stressdf$S-Stressdf$A)/2);
Stressdf$phi=atan(a/Stressdf$M);
Stressdf$G=Stressdf$M^2-Stressdf$N^2+Stressdf$Z*Stressdf$M-a*Stressdf$N;
Stressdf$H=2*Stressdf$M*Stressdf$N + a*Stressdf$M + Stressdf$Z*Stressdf$N;
#Stressdf$Mo = sqrt(Stressdf$r2-a^2); #removed due to non-use

pN = 3*Fn/(2*3.1416*a^3);
pT = 3*Ft/(2*3.1416*a^3);



Stressdf$sigNx = pN*((1+v1)*Stressdf$Z*Stressdf$phi+(1/Stressdf$r2)*((y^2-Stressdf$X^2)/(Stressdf$r2)*((1-v1)*Stressdf$N*Stressdf$Z^2-(1-2*v1)/3*(Stressdf$N*Stressdf$S+2*Stressdf$A*Stressdf$N+a^3)-v1*Stressdf$M*Stressdf$Z*a)-Stressdf$N*(Stressdf$X^2+2*v1*y^2)-Stressdf$M*Stressdf$X^2*Stressdf$Z*a/Stressdf$S));
Stressdf$sigNy = pN*((1+v1)*Stressdf$Z*Stressdf$phi+(1/Stressdf$r2)*((Stressdf$X^2-y^2)/(Stressdf$r2)*((1-v1)*Stressdf$N*Stressdf$Z^2-(1-2*v1)/3*(Stressdf$N*Stressdf$S+2*Stressdf$A*Stressdf$N+a^3)-v1*Stressdf$M*Stressdf$Z*a)-Stressdf$N*(y^2+2*v1*Stressdf$X^2)-Stressdf$M*y^2*Stressdf$Z*a/Stressdf$S));
Stressdf$sigNz = pN*(a*Stressdf$Z*Stressdf$M/Stressdf$S-Stressdf$N);
Stressdf$tauNxy = pN*(Stressdf$X*y*(1-2*v1)/Stressdf$r2^2*((2/3)*Stressdf$N*(Stressdf$S+2*Stressdf$A)-Stressdf$N*Stressdf$r2-Stressdf$Z*(Stressdf$Z*Stressdf$N+a*Stressdf$M)+(2/3)*a^3)+Stressdf$X*y*Stressdf$Z/Stressdf$r2^2*(a*Stressdf$M-a*Stressdf$M*Stressdf$r2/Stressdf$S-Stressdf$Z*Stressdf$N));
Stressdf$tauNyz = pN*((0-Stressdf$Z)*(y*Stressdf$N/Stressdf$S- y*Stressdf$Z*Stressdf$H/(Stressdf$G^2+Stressdf$H^2)));
Stressdf$tauNzx = pN*((0-Stressdf$Z)*(Stressdf$X*Stressdf$N/Stressdf$S-Stressdf$X*Stressdf$Z*Stressdf$H/(Stressdf$G^2+Stressdf$H^2)));

Stressdf$sigTx = pT*(-Stressdf$X*(1+0.25*v1)*Stressdf$phi + a*Stressdf$X*Stressdf$M/(Stressdf$r2^2) *((3/2-2*Stressdf$X^2/Stressdf$r2)*(Stressdf$S*v1-2*Stressdf$A*v1+Stressdf$Z^2)+(Stressdf$X^2)*(Stressdf$Z^2)/Stressdf$S +7*v1*Stressdf$r2/4-2*v1*Stressdf$X^2 +Stressdf$r2 ) + Stressdf$X*Stressdf$Z*Stressdf$N/Stressdf$r^4*((1.5-2*Stressdf$X^2/Stressdf$r2)*(-Stressdf$S/6*(1-2*v1)-Stressdf$A/3*(1-2*v1) -0.5*(Stressdf$Z^2+3*a^2) ) +a^2*Stressdf$X^2/Stressdf$S -v1*Stressdf$r2/4  -7*Stressdf$r2/4  )  + 4*a^3*Stressdf$X*Stressdf$Z/(3*Stressdf$r^4)*(1.5-2*Stressdf$X^2/Stressdf$r2)*(1-2*v1));
Stressdf$sigTy = pT * (-0.75*v1*Stressdf$X*Stressdf$phi                    + a*Stressdf$X*Stressdf$M/(Stressdf$r2^2)                   *((0.5-2*(y^2)/Stressdf$r2) *(v1*(Stressdf$S-2*Stressdf$A+Stressdf$r2)                        + Stressdf$Z^2)                      + y^2*(Stressdf$Z^2)/Stressdf$S                      + 0.75*v1*Stressdf$r2                    )                   + Stressdf$Z*Stressdf$X*Stressdf$N/(Stressdf$r2^2)                    *((0.5-2*(y^2)/Stressdf$r2)                      *(-Stressdf$S/6*(1-2*v1)                       -Stressdf$A/3*(1-2*v1)                        - 0.5*Stressdf$Z^2 - 1.5*a^2                      )+a^2*(y^2)/Stressdf$S-0.75*v1*Stressdf$r2  -Stressdf$r2/4 ) + (4/3)*a^3*Stressdf$Z*Stressdf$X/(Stressdf$r2^2)                   * (0.5-2*(y^2)/Stressdf$r2)  *(1-2*v1));
Stressdf$sigTz = pT*(Stressdf$Z*Stressdf$X*Stressdf$N/(2*Stressdf$r2)*(1-(Stressdf$r2+Stressdf$Z^2+a^2)/Stressdf$S));
Stressdf$sigTxy = pT*(0.5*y*(0.5*v1-1)*Stressdf$phi+a*y*Stressdf$M/(Stressdf$r2^2)*(Stressdf$X^2*Stressdf$Z^2/Stressdf$S+v1*((Stressdf$S-2*Stressdf$A)                                                                                *(0.5-2*Stressdf$X^2/Stressdf$r2)-2*Stressdf$X^2+Stressdf$r2/4)+Stressdf$r2/2+Stressdf$Z^2*(0.5-2*Stressdf$X^2/Stressdf$r2))                   +y*Stressdf$Z*Stressdf$N/Stressdf$r2^2*((0.5-2*Stressdf$X^2/Stressdf$r2)*((2*v1-1)*(Stressdf$S/6+Stressdf$A/3)                                                            -Stressdf$Z^2/2-3*a^2/2-Stressdf$r2/2)+Stressdf$r2*v1/4+a^2*Stressdf$X^2/Stressdf$S-y^2/2-3*Stressdf$X^2/2)                   +4*a^3*y*Stressdf$Z/(3*Stressdf$r2^2)*(0.5-2*Stressdf$X^2/Stressdf$r2)*(1-2*v1)); 
Stressdf$sigTyz = pT*Stressdf$X*y*Stressdf$Z/(2*Stressdf$r2^2)*(a*Stressdf$M*(0.5+1/Stressdf$S*(Stressdf$Z^2/2-3*a^2/2-Stressdf$r2/2))                                              +Stressdf$Z*Stressdf$N/2*(-3+1/Stressdf$S*(5*a^2+Stressdf$Z^2+Stressdf$r2)));
Stressdf$sigTzx = pT*(3*Stressdf$Z*Stressdf$phi/2+a*Stressdf$Z*Stressdf$M/Stressdf$r2*(1+Stressdf$X^2/Stressdf$r2-Stressdf$X^2/Stressdf$S)+Stressdf$N/Stressdf$r2                   *((-0.75)*(Stressdf$S+2*Stressdf$A)+Stressdf$Z^2-0.75*a^2-0.25*Stressdf$r2+Stressdf$Z^2/2*(0.5-2*Stressdf$X^2/Stressdf$r2)));

Stressdf$tauTxy = pT*(0.5*y*(0.5*v1-1)*Stressdf$phi+a*y*Stressdf$M/(Stressdf$r2^2)*(Stressdf$X^2*Stressdf$Z^2/Stressdf$S+v1*((Stressdf$S-2*Stressdf$A) *(0.5-2*Stressdf$X^2/Stressdf$r2)-2*Stressdf$X^2+Stressdf$r2/4)+Stressdf$r2/2+Stressdf$Z^2*(0.5-2*Stressdf$X^2/Stressdf$r2)) +y*Stressdf$Z*Stressdf$N/Stressdf$r2^2*((0.5-2*Stressdf$X^2/Stressdf$r2)*((2*v1-1)*(Stressdf$S/6+Stressdf$A/3)-Stressdf$Z^2/2-3*a^2/2-Stressdf$r2/2)+Stressdf$r2*v1/4+a^2*Stressdf$X^2/Stressdf$S-y^2/2-3*Stressdf$X^2/2) +4*a^3*y*Stressdf$Z/(3*Stressdf$r2^2)*(0.5-2*Stressdf$X^2/Stressdf$r2)*(1-2*v1)); 
Stressdf$tauTyz = pT*Stressdf$X*y*Stressdf$Z/(2*Stressdf$r2^2)*(a*Stressdf$M*(0.5+1/Stressdf$S*(Stressdf$Z^2/2-3*a^2/2-Stressdf$r2/2)) +Stressdf$Z*Stressdf$N/2*(-3+1/Stressdf$S*(5*a^2+Stressdf$Z^2+Stressdf$r2)));
Stressdf$tauTzx = pT*(3*Stressdf$Z*Stressdf$phi/2+a*Stressdf$Z*Stressdf$M/Stressdf$r2*(1+Stressdf$X^2/Stressdf$r2-Stressdf$X^2/Stressdf$S)+Stressdf$N/Stressdf$r2 *((-0.75)*(Stressdf$S+2*Stressdf$A)+Stressdf$Z^2-0.75*a^2-0.25*Stressdf$r2+Stressdf$Z^2/2*(0.5-2*Stressdf$X^2/Stressdf$r2)));



Stressdf$sigx = Stressdf$sigTx + Stressdf$sigNx;
Stressdf$sigy = Stressdf$sigTy + Stressdf$sigNy;
Stressdf$sigz = Stressdf$sigTz + Stressdf$sigNz;
Stressdf$tauxy = Stressdf$tauTxy + Stressdf$tauNxy;
Stressdf$tauyz = Stressdf$tauTyz + Stressdf$tauNyz;
Stressdf$tauzx = Stressdf$tauTzx + Stressdf$tauNzx;


Stressdf$sighs = (1/3)*(Stressdf$sigx+Stressdf$sigy+Stressdf$sigz);
Stressdf$sigvm = sqrt(0.5*(Stressdf$sigx-Stressdf$sigy)^2 + (Stressdf$sigy-Stressdf$sigz)^2+(Stressdf$sigx-Stressdf$sigz)^2+6*(Stressdf$tauxy^2+Stressdf$tauyz^2+Stressdf$tauzx^2));

VM=data.frame(Stressdf$X, Stressdf$Z, Stressdf$sigvm)
DFappend=data.frame(max(Stressdf$sigvm), mu, Fn)
write.table(VM, file=paste0(Note, "_", Fn, "_", mu, "_", "maxVM=",max(Stressdf$sigvm),".txt"),row.names=FALSE, na="",col.names=FALSE, sep=",")
#write.table(DFappend, file="Collection of Pyrex vs 2507 Hamilton VMstress.txt", row.names=FALSE, na="",col.names=FALSE,  sep=",",append=TRUE)

