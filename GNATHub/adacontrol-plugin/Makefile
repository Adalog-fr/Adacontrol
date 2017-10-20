BUILD_DIR := target
RESULTS_DIR := $(BUILD_DIR)

MVN            := BUILD_DIR=$(BUILD_DIR) mvn -PalternateBuildDir
MVN_OPTS       := -Xmx512m $(MAVEN_OPTS)
MVN_BUILD_OPTS := -Dmaven.test.skip=true
MVN_TEST_OPTS  :=

.PHONY: build

build:
	MAVEN_OPTS="$(MVN_OPTS)" $(MVN) $(MVN_BUILD_OPTS) package

set-version:
	MAVEN_OPTS="$(MVN_OPTS)" $(MVN) versions:set "-DnewVersion=$(SONAR_ADA_PLUGIN_VERSION)"

test:
	# The maven command can fail because of the tests, but we want the report to
	# be generated afterward.
	-MAVEN_OPTS="$(MVN_OPTS)" $(MVN) $(MVN_TEST_OPTS) package
	python surefire-to-gnatpython-report.py \
		-i $(BUILD_DIR)/surefire-reports \
		-o $(RESULTS_DIR)

clean:
	$(MVN) clean
