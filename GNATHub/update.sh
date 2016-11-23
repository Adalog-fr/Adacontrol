# Compilation of AdaControl gnathub and sonarqube plugins and 
# update of the environment
if [ "${1:-}" = "-h" ] ; then
    echo "Usage: update.sh "
    exit
fi

if [ "$OSTYPE" = "linux-gnu" ] ; then
    USRDIR=/usr/local/bin
    GNATDIR=/usr/gnat
    SONARDIR=/usr/sonarqube
    ADAROOT=/custom/donnees/profess/ada
    EXEC=
    # use standard Java
    # maven on standard path
else
    USRDIR=c:/applic/utils
    GNATDIR=`echo $GNAT |sed "s:\\\\\\\\:/:g"`
    GNATDIR=c:/applic/$GNATDIR
    SONARDIR=C:/Applic/sonarqube
    ADAROOT=C:/donnees/profess/ada
    EXEC=.exe
    JAVA_HOME=c:/Program\ Files/Java/jdk1.8.0_112
    PATH=/cygdrive/c/applic/apache-maven/bin:/cygdrive/c/Program\ Files/Java/jdk1.8.0_112/bin:$PATH
fi
adactl_dir=$ADAROOT/programmes/semtools/adacontrol
adactl_hub=${adactl_dir}/wks/GNATHub
sonarada=${adactl_dir}/gnatdashboard-18.0w-src/sonar-ada
sonarada_rules=${sonarada}/sonar-ada-plugin/src/main/java/org/sonar/plugins/ada/rules
sonarada_resources=${sonarada}/sonar-ada-plugin/src/main/resources

if [ -e ${adactl_dir}/wks/src/gen_adactl$$EXEC ] ; then
    echo ""
    echo "*** updating gen_adactl"
    mv -f ${adactl_dir}/wks/src/gen_adactl$EXEC $USRDIR
fi

echo ""
echo "***** regenerating xml files"
gen_adactl sonar.rules   ${adactl_dir}/wks/doc/adacontrol_ug.texi ${sonarada_resources}/adacontrol.xml
gen_adactl sonar.profile ${adactl_dir}/wks/doc/adacontrol_ug.texi ${sonarada_resources}/adacontrol-profile.xml

echo ""
echo "***** recompiling java plugins"
cp ${adactl_hub}/AdaControlRulesDefinitionXmlLoader.java ${sonarada_rules}
(cd $sonarada
 make
)
result=$?
if [ $result -ne 0 ] ; then
    echo "Make status for java plugins: $result"
    exit
fi

echo ""
echo "***** updating gnathub"
cp -v $adactl_dir/wks/GNATHub/*.py $GNATDIR/share/gnathub/extras

echo ""
echo "***** updating sonarqube"
cp -v ${sonarada}/sonar-ada-plugin/target/sonar-ada-plugin-*.jar   $SONARDIR/extensions/plugins/
