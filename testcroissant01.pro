pro testcroissant01,hfeet,kappa,alpha

nbp=500l
beta=lgen(nbp,-alpha,!pi/2)

c=-hfeet*sin(beta)/cos(alpha)

b=hfeet/cos(alpha)

oc=[[c*cos(beta)],[b+c*sin(beta)]]

r=kappa^2/(1.-kappa^2)*hfeet^2/cos(alpha)^2

plot,oc[*,0],oc[*,1],psym=3,/iso,xrange=[-4,4],yrange=[-2,6]

; -- plot OB
plots,[0,0],/data
plots,[0,b],/data,/continue,line=3


; -- plot OD
plots,[0,0],/data
plots,[hfeet*sin(alpha),hfeet*cos(alpha)],/data,/continue,line=2


; -- plot OE1
gamma=asin(kappa)
plots,[0,0],/data
plots,[hfeet/cos(gamma)*sin(alpha-gamma),hfeet/cos(gamma)*cos(alpha-gamma)],/data,/continue

; -- plot OE2
plots,[0,0],/data
plots,[hfeet/cos(gamma)*sin(alpha+gamma),hfeet/cos(gamma)*cos(alpha+gamma)],/data,/continue


; -- plot shell cross section
r=tan(gamma)*hfeet/cos(alpha)

for i=0l,nbp-1 do begin
  plots,oc[i,0]-r*cos(beta[i]),oc[i,1]-r*sin(beta[i]),psym=1,/data
  plots,oc[i,0]+r*cos(beta[i]),oc[i,1]+r*sin(beta[i]),psym=1,/data

endfor





return
end