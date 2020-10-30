include/images/readme.txt			12/1/02				tlwms

Please update this documentation if you change the image configuration.
Thanks!

When using Qt Designer to add an icon to a toolbar button, the image
is extracted out of the selected xpm file and embedded as data in
the .ui file for the project.  The UIC program converts this data into
a static char ** definition embedded in the generated .cpp file.
Note that changing the image in the xpm file in this dir has no effect
on the image used by the application!

Some widgets are generated programatically, primarily for user-defined
classes such as ATable or APage.  The icons for these cases are implemented
in a different way.  A #include foo.xpm statement in the code causes
a static const char** definition to be inserted.  This static array of
pointers is used in the initialization of a Qt pixmap that is, in turn,
placed into a toolbar button or other widget.  In this case, the
include file in this directory is referenced on every compile of the
webide project.  Changing the following xpm files will cause the
image to be modified on the next build of webide.

See iconconvert/readme.txt for information on how to construct/modify xpm files.

The following xpm files are referenced by #include entries in abasedev code:
Lambdalist.XPM
check_off.XPM
check_on.XPM
comment.XPM
document.XPM
drive.XPM
enginebusy.XPM
enginenotbusy.XPM
fileclose.XPM
filenew.XPM
fileopen.XPM
filesave.XPM
folderclosed.XPM
folderup.XPM
indent.XPM
uncomment.XPM
unindent.XPM
errortrace.xpm
instructiontrace.xpm
syscheck.xpm
go.xpm    -- only for the image used in the console tab, used elsewhere as a one time import
stop.xpm  -- only for the image used in the console tab, used elsewhere as a one time import
