;+
; PURPOSE:
;  Parse the model param structure returned by getmoddefparam.pro
; 
; CATEGORY:
;  simulation, raytracing, data handling
;
; INPUTS:
;  ss : structure returned by getmoddefparam.pro
;
; OUTPUTS:
;  return,modparam : model parameter array
;  sv : structure with each of the default parameters
;  comarr : command array used to build the modparam
;  moddesc : model description from first line of structure s,
;            extracted if present.
;
; KEYWORDS:
;  filepro : a procedure to build modparam will be generated and
;            saved in filepro. It can be reused to generate modparam
;            with different parameters. That function needs the
;            structure sv as an input.
;  verb : print some blabla during execution
;  
; CVS:
;  $Id: parsemoddefparam.pro,v 1.1 2006-09-08 16:03:21 nathan Exp $
;-


function parsemoddefparam,ss,sv,comarr,filepro=filepro,verb=verb,moddesc=moddesc

verb=keyword_set(verb)

; -- init so that it exits with the right value
moddesc='No description available.'

; ---- extract default parameter embedded structure
s=ss.s

; ---- check input
if size(s,/type) ne 8 then begin
    message,'Input does not look like a structure.',/info
    return,0.
endif


nbelem=n_elements(s)
 
; ---- look for title presence: should be the first elem of the structure
if s[0].keyword eq '' then begin
    flagmoddesc=1B
    moddesc=s[0].def
endif else flagmoddesc=0B

; ---- get the size of the parameter definition
idx=0L
while (s[idx].def ne '' and idx lt nbelem-1) do idx++
idxstartcode=idx

if idx eq nbelem-1 then begin
    flagnoprog=1b 
    idxstartcode++
endif else flagnoprog=0b

; ---- read the parameters
; -- set to 0 if no parameters needed
if idxstartcode eq (0+flagmoddesc) then begin
    sv=0
    flagemptysv=1b
endif else begin
    flagemptysv=0b
    com='sv={'
    for i=0L + flagmoddesc,idxstartcode-2 do com+=s[i].keyword+':'+s[i].def+','
    com+=s[i].keyword+':'+s[i].def+'}'
    r=execute(com)
    if not r then message,'Execution of "'+com+'" failed !'
endelse


if flagnoprog and flagemptysv and flagmoddesc then begin
    message,'Only model description is present.',/info
    return,0.
endif


; ---- test if list of statement required
if flagnoprog then begin
    ; ---- no special statements to build the array of parameter
    com='modparam=['
    for i=0L,n_tags(sv)-2 do com+='sv.('+strtrim(i,2)+'),'
    com+='sv.('+strtrim(i,2)+')]'
    
    if not execute(com) then $
      message,'Execution of "'+com+'" failed !'
    
    comarr=com
    descarr=strarr(n_elements(com))
    descarr[0]='Build model parameter array.'
endif else begin
    ; ---- some code is needed to build modparam
    nbcom=n_elements(s)-idxstartcode-1
    comarr=strarr(nbcom)
    descarr=strarr(nbcom)
    idx=0L

    ; -- don't try replacing parameters if none are present
    if not flagemptysv then begin
        
        svtagnames=strlowcase(tag_names(sv))
        perlscript='#!/usr/bin/perl'
        
        for j=0,n_tags(sv)-1 do begin
            jstr=strtrim(j,2)
            ; -- It should be possible to do that in IDL 
            ;    but it's easier for me in perl
            perlscript=[perlscript,'$'+svtagnames[j]+'="sv.('+jstr+')";']
        endfor

        for i=idxstartcode+1,n_elements(s)-1 do begin
            ; -- replace by the variable name
            perlscript=[perlscript,'print "'+s[i].def+'\n";']

            ; -- get the pretty comments
            descarr[idx++]=s[i].desc
        endfor

        savetxtfile,'perlscript.pl',perlscript
        spawn,'chmod 755 perlscript.pl'
        spawn,'./perlscript.pl',comarr
        
    endif else begin
        for i=idxstartcode+1,n_elements(s)-1 do begin
            ; -- replace by the variable name
            comarr[idx]=s[i].def
            
            descarr[idx++]=s[i].desc
            
        endfor
    endelse
        
endelse

; ---- execute the IDL script to build modparam
if n_elements(filepro) ne 0 then begin
    ; ---- save in as a IDL pro if requested
    fn=filepro
    check_filename,fn,pathima,extima
endif else begin
    fn='idlscript'
    extima='pro'
    pathima=''
    filepro=fn+'.'+extima
endelse

; -- header of the script
proglisting=['; Generated by parsemoddefparam.pro',$
             '; For density model : '+strtrim(ss.modelid,2),$
             '; Model description: '+moddesc,$
             'function '+fn+',sv']


; -- core of the script
for i=0,n_elements(comarr)-1 do proglisting=[proglisting,comarr[i]+'     ; '+descarr[i]]
if not flagnoprog then $
  proglisting=[proglisting,$
               'modparam='+s[idxstartcode].keyword]
; -- end the script
proglisting=[proglisting,'return,modparam','end']

; -- save
savetxtfile,filepro,proglisting
; -- compile
resolve_routine,fn,/is_function
; -- execute
modparam=call_function(fn,sv)


return,modparam
end
