
ima=sccreadfits('20100618_080921_s4h2A.fts',hdra)
imb=sccreadfits('20100618_080921_s4h2B.fts',hdrb)


; rtinitenv,forcelibfile='/home/thernis/tmp03/scraytrace-2.5.2/src/.libs/libraytrace.so',forcelibthread='/home/thernis/tmp03/scraytrace-2.5.2/src/.libs/libraytracethread.so'
  

; rtinitenv,forcelibfile='/home/thernis/work/cpp/scraytrace/opti64_UbuntBoost/src/.libs/libraytrace.so',forcelibthread='/home/thernis/work/cpp/scraytrace/opti64_UbuntBoost/src/.libs/libraytracethread.so'

rtinitenv, forcelibfile='/home/thernis/work/cpp/fromgit/SCRaytrace/build/src/libraytrace.so', forcelibthread='/home/thernis/work/cpp/fromgit/SCRaytrace/build/src/libraytracethread.so'




rtsccguicloud,ima,imb,hdra,hdrb,maxheight=200.

sa0=sccreadfits('/net/cronus/opt/secchi/lz/L0/a/img/cor2/20081212/20081212_073754_d4c2A.fts')
sb0=sccreadfits('/net/cronus/opt/secchi/lz/L0/b/img/cor2/20081212/20081212_073754_d4c2B.fts')
sa=sccreadfits('/net/cronus/opt/secchi/lz/L0/a/img/cor2/20081212/20081212_140754_d4c2A.fts',hdrA)
sb=sccreadfits('/net/cronus/opt/secchi/lz/L0/b/img/cor2/20081212/20081212_140754_d4c2B.fts',hdrB)
ima=bytscl(alog10(rebin(sa-sa0,512,512) > 1e4 < 1e6))
imb=bytscl(alog10(rebin(sb-sb0,512,512) > 1e4 < 1e6))
pim = list(ima,imb)
phdr = list(hdrA,hdrB)
rtcloudwidget,pim=pim,phdr=phdr,ssim=ssim,sgui=sgui,swire=swire,ocout=ocout,ldisp=ldisp;,/itool



sa0 = sccreadfits('/net/cronus/opt/secchi/lz/L0/a/img/cor2/20181102/20181102_002400_d4c2A.fts')
sa = sccreadfits('/net/cronus/opt/secchi/lz/L0/a/img/cor2/20181102/20181102_005400_d4c2A.fts', hdrA)
ima = bytscl(alog10(rebin(float(sa) - float(sa0), 512, 512) > 2e-1 < 9e0))

c20 = sccreadfits('/net/corona/lasco/lz/level_05/181102/c2/22700987.fts', /LASCO)
c2 = sccreadfits('/net/corona/lasco/lz/level_05/181102/c2/22700991.fts', /LASCO, hdrc2)
imc2 = bytscl(alog10(rebin(float(c2) - float(c20), 512, 512) > 1e0 < 5e1))


c30 = sccreadfits('/net/corona/lasco/lz/level_05/181102/c3/32560371.fts', /LASCO)
c3 = sccreadfits('/net/corona/lasco/lz/level_05/181102/c3/32560375.fts', /LASCO, hdrc3)
imc3 = bytscl(alog10(rebin(float(c3) - float(c30), 512, 512) > 1e0 < 5e1))

; wispreadfits, '/net/ares/pub/wispr/fm/ql/Images/fits/L2/20181101/psp_L2_wispr_20181101T231550_1221.fits', hdrw0, im0
; wispreadfits, '/net/ares/pub/wispr/fm/ql/Images/fits/L2/20181102/psp_L2_wispr_20181102T004550_1221.fits', hdrW, im

wispreadfits, '/net/ares/pub/wispr/fm/ql/fits/L2/20181101/psp_L2_wispr_20181101T231550_1221.fits', hdrw0, im0
wispreadfits, '/net/ares/pub/wispr/fm/ql/fits/L2/20181102/psp_L2_wispr_20181102T004550_1221.fits', hdrW, im



imw = bytscl(alog10(rebin(float(im) - float(im0), 480, 512) > 1e-13 < 1e-12))


pim = list(ima, imc2, imc3, imw)
phdr = list(hdrA, hdrc2, hdrc3, hdrW)
rtcloudwidget,pim=pim,phdr=phdr,ssim=ssim,sgui=sgui,swire=swire,ocout=ocout,ldisp=ldisp


pp = image(imw)

p = image(ima)
p = image(imc2)
p = image(imc3)
p = image(imw)



pim=[ptr_new(ima),ptr_new(imb)]
phdr=[ptr_new(hdrA),ptr_new(hdrB)]


; ---- display test
owin = obj_new('IDLgrWindow', dimension=[512,512], title='Test', retain=2, renderer=1)
owin = obj_new('IDLgrWindow', dimension=[512,512], title='Test')

owin = obj_new('IDLgrWindow', dimension=[512,512], title='Test', retain=2, renderer=1)

oview = obj_new('IDLgrView', view=[0,0,512,512], color=[0,0,0])
oidlgrmodel = obj_new('IDLgrModel')
oimage = obj_new('IDLgrImage', bytscl(dist(512)))

oview->add,oidlgrmodel,position=0
oidlgrmodel->add,oimage
owin->draw,oview




end