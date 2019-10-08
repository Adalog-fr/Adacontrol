# Compilation of AdaControl gnathub and sonarqube plugins
if [ "${1:-}" = "-h" ] ; then
    echo "Usage: update.sh "
    exit
fi

if [ "$OSTYPE" = "linux-gnu" ] ; then
    ### LINUX
    ADAROOT=/donnees/profess/ada
    EXEC=
    # use standard Java
    # maven on standard path
else
    ### WINDOWS
    ADAROOT=C:/donnees/profess/ada
    EXEC=.exe
    JAVA_HOME=c:/Program\ Files/Java/jdk-11.0.4
    PATH=/cygdrive/c/applic/apache-maven/bin:/cygdrive/c/Program\ Files/Java/jdk-11.0.4/bin:$PATH
fi
adactl_dir=$ADAROOT/programmes/semtools/adacontrol
adactl_hub=${adactl_dir}/wks/GNATHub
sonarada=${adactl_hub}/adacontrol-plugin
sonarada_resources=${sonarada}/src/main/resources

echo "***** regenerating xml files"
gen_adactl sonar.rules   ${adactl_dir}/wks/doc/adacontrol_ug.texi ${sonarada_resources}/adacontrol.xml
gen_adactl sonar.profile ${adactl_dir}/wks/doc/adacontrol_ug.texi ${sonarada_resources}/adacontrol-profile.xml

echo ""
echo "***** recompiling java plugins"
(cd $sonarada
 make
)
result=$?
if [ $result -ne 0 ] ; then
    echo "Make status for java plugins: $result"
    exit
fi

echo "***** building zip file"
rm -f AdacontrolDashboard.zip
zip -j9 AdacontrolDashboard.zip $sonarada/target/adacontrol-plugin-*.jar adacontrol.py
