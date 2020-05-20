CV_DATA_ROLE_1=$(shell find data-role-cv -type f -name "*data.RData" | grep -vP -- "-(keras|lstm|lstmdist)_")
CV_DATA_ROLE_2=$(shell find data-role-cv -type f -regextype "egrep" -regex ".*5-300.*(keras|lstm|lstmdist)_.*data.RData" | sed -r "s%data-role-cv/study-role-cv-([0-9]+).*epochs_([0-9]+)(_.*)%\1\t\2\t\0%g" | sort -g -k2,2 -k1,1 | cut -f 3)
CV_DATA_ROLE_3=$(shell find data-role-cv -type f -regextype "egrep" -regex ".*(keras|lstm|lstmdist)_.*data.RData" | sed -r "s%data-role-cv/study-role-cv-([0-9]+).*epochs_([0-9]+)(_.*)_seed_([0-9]+)%\1\t\2\t\4\t\0%g" | sort -g -k2,2 -k3,3 -k1,1 | cut -f 4)

CV_RESULTS_ROLE_1=$(patsubst %-data.RData,%-result.RData,$(CV_DATA_ROLE_1))
CV_RESULTS_ROLE_2=$(patsubst %-data.RData,%-result.RData,$(CV_DATA_ROLE_2))
CV_RESULTS_ROLE_3=$(patsubst %-data.RData,%-result.RData,$(CV_DATA_ROLE_3))


.PHONY: all
all:	
	$(info -- calculations are not repeated)

study-addressee-draftrf.RData: ../scripts/study-addressee.R
	CALCULATE_NONDRAFT=ON DRAFT_RF=ON WRITE_OUT=OFF Rscript ../scripts/study-addressee.R

logging-%:
	mkdir logging-$*

.SECONDARY: logging-%

study-addressee-rf-%.rds: logging-% study-addressee-draftrf.RData ../scripts/study-addressee-rf.R
	CALCULATE_NONDRAFT=ON ADDRESSEE_RF_VARSET=$* Rscript ../scripts/study-addressee-rf.R

study-addressee-workspace.RData: study-addressee-rf-Speech.rds study-addressee-rf-Visual.rds study-addressee-rf-Observable.rds study-addressee-rf-All.rds ../scripts/study-addressee.R
	CALCULATE_NONDRAFT=ON DRAFT_RF=OFF LOAD_RF=ON WRITE_OUT=ON Rscript ../scripts/study-addressee.R

data.info_study-addressing-apartment.ini: study-addressee-workspace.RData ../scripts/study-addressee-plots.R ../scripts/tikz-export-config.R
	CALCULATE_NONDRAFT=ON DRAFT_RF=OFF LOAD_RF=ON WRITE_OUT=ON Rscript ../scripts/study-addressee-plots.R

study-meka-workspace.RData: ../scripts/study-meka.R study-meka.csv
	CALCULATE_NONDRAFT=ON WRITE_OUT=ON Rscript ../scripts/study-meka.R

data.info_meka.ini: study-meka-workspace.RData ../scripts/study-meka-plots.R ../scripts/tikz-export-config.R
	CALCULATE_NONDRAFT=ON WRITE_OUT=ON Rscript ../scripts/study-meka-plots.R

study-group-workspace.RData: ../scripts/study-group.R group_match_grid_search.tsv.gz group_match_statistics.tsv.gz group_face_detections.tsv.gz role_annotations.tsv.gz
	CALCULATE_NONDRAFT=ON WRITE_OUT=ON Rscript ../scripts/study-group.R

data.info_group.ini: study-group-workspace.RData ../scripts/study-group-plots.R ../scripts/tikz-export-config.R
	CALCULATE_NONDRAFT=ON WRITE_OUT=ON Rscript ../scripts/study-group-plots.R

study-role-data-full.RData: ../scripts/study-role-prepare.R study-group-workspace.RData
	mkdir -p data-role-cv
	CALCULATE_NONDRAFT=ON Rscript ../scripts/study-role-prepare.R

data-role-cv/study-role-cv-%-result.RData: data-role-cv/study-role-cv-%-data.RData
	./study-role-eval-model.sh "$<"

study-role-eval-performance-data.RData: ../scripts/study-role-collect.R $(CV_RESULTS_ROLE_1) $(CV_RESULTS_ROLE_3)
	Rscript ../scripts/study-role-collect.R

data.info_role.ini: ../scripts/study-role.R study-role-data-full.RData study-role-eval-performance-data.RData ../scripts/tikz-export-config.R
	CALCULATE_NONDRAFT=ON WRITE_OUT=ON Rscript ../scripts/study-role.R

.PHONY: calculations-addressee
calculations-addressee: data.info_study-addressing-apartment.ini
	$(info -- addressee calculations finished)

.PHONY: calculations-meka
calculations-meka: data.info_meka.ini
	$(info -- meka calculations finished)

.PHONY: calculations-group
calculations-group: data.info_group.ini
	$(info -- group calculations finished)

.PHONY: calculations-role
calculations-role: data.info_role.ini
	$(info -- role calculations finished)

.PHONY: calculations
calculations: calculations-addressee calculations-meka calculations-group calculations-role
	$(info -- role calculations finished)
