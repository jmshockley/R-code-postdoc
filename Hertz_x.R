#USER-DEFINED MATERIAL PROPERTIES
R1=Inf;        #Flat surface
E1= 117*10^9;   #Elastic modulus of flat surface in N/m^2 (Pa)
v1= .34;        #Poisson Ratio of flat surface

R2= 0.003175;    #Radius of ball/hemispehre in m
E2= 345*10^9;          #Elastic modulus of ball/hemisphere in N/m^2 (Pa)
v2= .3;          #Poisson Ratio of ball/hemisphere
Fn= 5;          #Normal load (N)

mu= 0.4;          #Coefficient of Friction 
Ft= mu*Fn ;    #Tangential load

#COMPUTATION OF HERTZIAN STRESS
R=1/(1/R1+1/R2);                    #Reduced Radius
E=1/((1-v1^2)/E1 + (1-v2^2)/E2);    #Reduced Modulus
a= ((3*Fn*R2)/(4*E))^(1/3);         #Contact half-width (m)
#x=seq(from=-2*a, to=2*a,by=0.004*3*a);               #Discrete values of x-axis (m)
#z=seq(from=0, to=2*a, by=0.002*3*a);                 #Discrete values of z-axis (m)
x=seq(from=-100*10^-6, to=100*10^-6, by=1.01*10^-6); #-100*10^-6:1.01*10^-6:100*10^-6;     #Standard size of x
z=seq(from=0, to=100*10^-6, by=1.01*10^-6)             #Standard size of z


y=0; #-100*10^-6;                #Value of y due to semi-infinite plane
Ac=3.1416*a^2;                      #Hertzian Contact Area (m^2)
P=Fn/Ac;                            #Average Hertzian Pressure (N/m^2)
Po=3*P/2;                           #Maximum Hertzian Pressure (N/m^2)


Xvalues=rep(x,length(x))
Zvalues=sort(rep(z,length(z)))
Stressdf = data.frame(Xvalues, Zvalues)
