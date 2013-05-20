cd %~dp0
chcp 65001
cmd /K java.exe -DrootFolder="E:\BNTWorld" -Dverbose=true -DtestNova=true -DtestHorizont=true -Dvlc="C:\Program Files\VideoLAN\VLC\vlc.exe" -Dfile.encoding=UTF-8 -Xbootclasspath/p:.\libs\scala-library.jar -classpath .\libs\tagsoup-1.2.jar;.\libs\log4j-1.2.17.jar;.\libs\AVRecorder.jar nasko.avrecorder.Scheduler