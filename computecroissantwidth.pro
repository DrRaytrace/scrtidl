
; ------ compute the width of the croissant model
; INPUTS:
;  halfangle_rad : model half angle, in radian
;  leadingEdgeHeight_Rsun : the leading egde height, in Rsun (this should work with whatever units actually, but then the results will be in the units you used)
;  aspectRatio : aspect ration of the model
;
; OUPUTS:
;  plot a cross cut of the model
;  print,the half width on screen

pro computecroissantwidth,halfangle_rad,leadingEdgeHeight_Rsun,aspectRatio

alpha=halfangle_rad
k=aspectRatio

hf=leadingEdgeHeight_Rsun*(1.-k)*cos(alpha)/(1.+sin(alpha))


h=hf/cos(alpha)
rho=hf*tan(alpha)

nbbeta=1000l
beta=lgen(nbbeta,-alpha,!pi/2)

X0=(rho+h*k^2*sin(beta))/(1-k^2)
rc=sqrt((h^2*k^2-rho^2)/(1-k^2)+X0^2)
cac=beta

op2=[[(X0+rc)*cos(beta)],[(X0+rc)*sin(beta)]]
op=[[X0*cos(beta)],[X0*sin(beta)]]
op1=[[(X0-rc)*cos(beta)],[(X0-rc)*sin(beta)]]

; ---- find X derivative
op2_x=(X0+rc)*cos(beta)
deltabeta=beta[0]-beta[1]
derive=(op2_x-shift(op2_x,1))/deltabeta


wnd,0,800,600
plot,beta[1:*],derive[1:*]

; -- assumes positive slope and monotonous
mposit=min(where(derive ge 0.,cntposit))
mnegat=max(where(derive lt 0.,cntnegat))

slope=(derive[mnegat]-derive[mposit])/deltabeta
intercept=(derive[mposit]*beta[mnegat]-derive[mnegat]*beta[mposit])/deltabeta

betamax=-intercept/slope

print,'betamax [rad] : ',betamax

plots,betamax,0,/data,psym=1,symsize=2


X0max=(rho+h*k^2*sin(betamax))/(1-k^2)
rcmax=sqrt((h^2*k^2-rho^2)/(1-k^2)+X0max^2)
maxhalfwidth=(X0max+rcmax)*cos(betamax)
heightatmax=(X0max+rcmax)*sin(betamax)+h


wnd,1,800,1000
plot,op2[*,0],op2[*,1]+h,/iso
oplot,op[*,0],op[*,1]+h,line=1
oplot,op1[*,0],op1[*,1]+h

plots,[0,0],/data
plots,op2[0,0],op2[0,1]+h,/data,/continue

plots,[0,0],/data
plots,op[0,0],op[0,1]+h,/data,/continue,line=1

plots,[0,0],/data
plots,op1[0,0],op1[0,1]+h,/data,/continue

plots,maxhalfwidth,heightatmax,/data,psym=2,symsize=2

print,'maxhalfwidth [Rsun] : ',maxhalfwidth
print,'heightatmax [Rsun] : ',heightatmax



return
end