##### Installation Steps #####
# 1. Install Oracle InstantClient - https://www.oracle.com/database/technologies/instant-client/macos-intel-x86-downloads.html - basic and SDK
# 2. Copy contents of SDK in basic
# 3. Refer https://gist.github.com/datalove/05e7dde56284ea9e9a00 for next steps
# 4. Download ROracle - https://www.oracle.com/database/technologies/roracle-downloads.html 
# 5. On terminal, run: R CMD INSTALL --configure-args='--with-oci-lib=/opt/oracle/instantclient_19_3' ROracle_1.3-2.tar.gz
# 6. Set hostname in etc/hosts


##### Connect #####
library(ROracle)

drv <- dbDriver("Oracle")

host <- "obieedev-scan.amdc.mckinsey.com"
port <- 1521
service_name <- "obdatdev_srvs"

connect.string <- paste("(DESCRIPTION=",
                        "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
                        "(CONNECT_DATA=(SERVICE_NAME=", service_name, ")))", 
                        sep = "")

con <-dbConnect(drv, 
                username = "XXMCK_CLIENTSVC", 
                password = "c1i3nt4svc4uat",
                dbname=connect.string)


mdl_alpha_host <- "mdl-alpha-cpat-spint-new.cao5jroudepg.us-east-1.rds.amazonaws.com"
mdl_alpha_port <- 1521
mdl_alpha_service_name <- "SPINT"
mdl_alpha_username = "CDM_READER"
mdl_alpha_password = "Ed0i2rsyR0OC"
con_mdl_alpha <- connect_to_oracle_db(mdl_alpha_host, mdl_alpha_port, mdl_alpha_service_name, mdl_alpha_username, mdl_alpha_password)

##### Explore #####
dbGetQuery(con,"select count(*) from XXMCK_CLIENTSVC.ED_PARTY_MERGED_V_S_PROD")

d <- dbReadTable(con, "ED_PARTY_MERGED_V_S_PROD")



name = str_to_upper("FDW_DATA_EXPORT_PROD")
ochargeCode_data <- tbl(con, name) %>% 
  collect()

ocdm_data <- tbl(con, cdm_table_name)

cdm_capiq_mapping <- ocdm_data %>%
  dplyr::filter(STATUS = "A") %>%
  dplyr::select(PARTY_ID) %>%
  left_join(
    dplyr::select(ocdm_mappings_data, PARTY_ID, PARTY_IDENTIFIER_VALUE ) %>%
      filter(
        (ISSUING_AUTHORITY_NAME == "CAPIQ") &
          (PARTY_IDENTIFIER_VALUE %like% "^[[:digit:]]+$")
      ),
    by="PARTY_ID"
  ) %>%
  collect()