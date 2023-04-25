.upd.lib:
         UPD $*.for $*.ser /s
         UPD $*.for
         FOR1 update.out,$*;
         PAS2
         LIB HYPOXIA

hypoxia.lib: atco.upd

hypoxia.lib: blco2.upd

hypoxia.lib: blo2.upd

hypoxia.lib: calck.upd

hypoxia.lib: chart.upd

hypoxia.lib: chart2.upd

hypoxia.lib: chart3.upd

hypoxia.lib: chrtok.upd

hypoxia.lib: credit.upd

hypoxia.lib: csh.upd

hypoxia.lib: def.upd

hypoxia.lib: diff.upd

hypoxia.lib: farhi.upd

hypoxia.lib: gilbert.upd

hypoxia.lib: gtchrt.upd

hypoxia.lib: hfile.upd

hypoxia.lib: hlist.upd

hypoxia.lib: hypbd.upd

hypoxia.lib: hypdel.upd

hypoxia.lib: hyplot.upd

hypoxia.lib: hyplst.upd

hypoxia.lib: hypxmn.upd

hypoxia.lib: mit.upd

hypoxia.lib: o2diss.upd

hypoxia.lib: read.upd

hypoxia.lib: report.upd

hypoxia.lib: root.upd

hypoxia.lib: setngo.upd

hypoxia.lib: write.upd

hypoxia.lib: write2.upd

hypoxia.obj: hypoxia.upd
             UPD $*.for
             FOR1 update.out,$*,$*.lst;
             PAS2

hypoxia.lib: hypoxia.obj
             LINK $*,b:,/e,b:$* a:fortran a:math;

hypoxia.exe: hypoxia.lib
             LINK $*,b:,/e,b:$* a:fortran a:math;


