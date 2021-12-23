#prepare 
function RemoveDir() {

  echo 'removing '$1
  rm -rf $1
  echo $1' removed'
}
function clearResoures() {
  RemoveDir 'pesp'
  RemoveDir 'HashLib4Pascal'
}

function cloneRepo() {
  git clone $1
  return $?
} 

cloneRepo 'https://github.com/bogomolov-a-a/pesp.git' 
cloneRepo 'https://github.com/bogomolov-a-a/HashLib4Pascal.git'

if [ $? -ne 0 ];
then 
  clearResoures
  exit 1;
fi
#build
../lazarus/share/lazarus/lazbuild -B -r --verbose --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 freepascal-application-common/src/main/freepascalapplicationcommon.lpk
if [ $? -ne 0 ];
then 
  clearResoures
  exit 1;
fi
#--add-package pesp/src/main/pesp.lpk --add-package HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk --add-package freepascal-application-common/src/main/freepascalapplicationcommon.lpk 
../lazarus/share/lazarus/lazbuild -B -r --verbose --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 freepascal-addition-section-manipulator/src/main/custapp.addionalsectionmanipulator.lpk
if [ $? -ne 0 ];
then 
  clearResoures
  exit 1;
fi
#../lazarus/share/lazarus/lazbuild -B -r --verbose --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 freepascal-application-signature-helper/src/custapp.helper.signature.lpk
#if [ $? -ne 0 ];
#then 
#  clearResoures
#  exit 1;
#fi
#../lazarus/share/lazarus/lazbuild -B -r --verbose --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 --add-package pesp/src/main/pesp.lpk --add-package freepascal-addition-section-manipulator/src/main/custapp.addionalsectionmanipulator.lpk freepascal-map-file-based-logger/src/mapbasedfilelogger.lpk
#if [ $? -ne 0 ];
#then 
#  clearResoures
#  exit 1;
#fi
#tests
#../lazarus/share/lazarus/lazbuild -B -r --verbose --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 --add-package freepascal-addition-section-manipulator/src/main/custapp.addionalsectionmanipulator.lpk --add-package freepascal-map-file-based-logger/src/mapbasedfilelogger.lpk --add-package pesp/src/main/pesp.lpk freepascal-map-file-based-logger/src/logger-section-writer/LogSectionWriter.lpi
#if [ $? -ne 0 ];
#then 
#  clearResoures
#  exit 1;
#fi
clearResoures
exit 0
