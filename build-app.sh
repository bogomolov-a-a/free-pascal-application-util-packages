#prepare 
git clone git@github.com:bogomolov-a-a/pesp.git

../lazarus/share/lazarus/lazbuild -B -r --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 --add-package pesp/src/main/pesp.lpk freepascal-addition-section-manipulator/src/custapp.addionalsectionmanipulator.lpk
if [ $? -ne 0 ];
then 
  exit 1;
fi
../lazarus/share/lazarus/lazbuild -B -r --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 freepascal-application-signature-helper/src/custapp.helper.signature.lpk
if [ $? -ne 0 ];
then 
  exit 1;
fi
../lazarus/share/lazarus/lazbuild -B -r --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 --add-package pesp/src/main/pesp.lpk --add-package freepascal-addition-section-manipulator/src/custapp.addionalsectionmanipulator.lpk freepascal-map-file-based-logger/src/mapbasedfilelogger.lpk
if [ $? -ne 0 ];
then 
  exit 1;
fi
#tests
../lazarus/share/lazarus/lazbuild -B -r --lazarusdir=/root/lazarus/share/lazarus --compiler=/root/fpc/ppcx64 --add-package freepascal-addition-section-manipulator/src/custapp.addionalsectionmanipulator.lpk --add-package freepascal-map-file-based-logger/src/mapbasedfilelogger.lpk --add-package pesp/src/main/pesp.lpk freepascal-map-file-based-logger/src/logger-section-writer/LogSectionWriter.lpi
if [ $? -ne 0 ];
then 
  exit 1;
fi
exit 0
