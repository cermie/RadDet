#!/usr/bin/env bash
f=false
if !((test -e cla.o) && (test -e kinds.o) && (test -e kinds.mod) && (test -e cla.mod)); then
    echo "compiling cla"
    gfortran -c kinds.f90     
    gfortran -c cla.f90
    f=true
fi
if !((test -e cla.o) && (test -e kinds.o) && (test -e kinds.mod) && (test -e cla.mod)); then
    exit
fi
if !(test -e spline.o); then
    echo "compiling spline"
    gfortran -c spline.f90
fi
fl2=false
if !((test -e splines.o) && (test -e splines.mod)); then
    echo "compiling splines"
    gfortran -c splines.f90     
    fl2=true
fi
fl=false
if !((test -e data.o) && (test -e data.mod)) || (test data.f90 -nt data.o); then
    echo "compiling data"
    gfortran -c data.f90     
    fl=true
fi
if !(test -e CS.o) || (test CS.f90 -nt CS.o) || !(test -e crossections.mod); then
    echo "compiling crossections"
    fl=true
    gfortran -c CS.f90
fi
if !(test -e kernel_prep.o) || (test kernel_prep.f90 -nt kernel_prep.o) || !(test -e kernel_prep.mod) || $fl; then
    echo "compiling kernel_prep"
    fl=true
    gfortran -c kernel_prep.f90
fi
if !(test -e rev_task.o) || (test rev_task.f90 -nt rev_task.o) || !(test -e reverse.mod) || $fl; then
    echo "compiling reverse"
    fl=true
    gfortran -c rev_task.f90
fi
if !(test -e radet.f90) || (test radet.f90 -nt radet.o) || $fl || $f || $fl2; then
    echo "compiling radet"
    fl=true
    gfortran -c radet.f90  
fi
echo "Linking..."
gfortran -o radet radet.o CS.o kernel_prep.o rev_task.o cla.o kinds.o data.o spline.o splines.o
echo "Done"
