learning2014 = read.table("e2.txt",  header = TRUE, sep = "\t", colClasses = "character")

summary(learning2014)

# Questions are answered 1-5 scale, except four last questions
# There is 183 person answered to the survey

gender <- c(learning2014$gender)

for(i in c(1,3:ncol(learning2014))) {
  learning2014[,i] <- as.numeric(as.character(learning2014[,i]))
}
learning2014$GENDER <- gender

# deep = Deep = d_sm+d_ri+d_ue
learning2014$deep <- (learning2014$D03 + learning2014$D11 + learning2014$D19 + learning2014$D27 + learning2014$D07 + learning2014$D14 + learning2014$D22 + learning2014$D30 + learning2014$D06 + learning2014$D15 + learning2014$D23 + learning2014$D31) /12 

# d_sm      D03 + D11 + D19 + D27
# *Seeking Meaning
# d_ri      D07 + D14 + D22 + D30
# *Relating Ideas
# d_ue      D06 + D15 + D23 + D31
# *Use of Evidence

# stra = Strategic = st_os + st_tm

learning2014$stra <- (learning2014$ST01 + learning2014$ST04 + learning2014$ST09 +
                        learning2014$ST12 + learning2014$ST17 + learning2014$ST20 +
                        learning2014$ST25 + learning2014$ST28) /8

# st_os     ST01 + ST09 + ST17 + ST25
#*Organized Studying
#st_tm     ST04 + ST12 + ST20 + ST28
#*Time Management

# surf = Surface = su_lp + su_um + su_sb
learning2014$surf <- (learning2014$SU02 + learning2014$SU05 + learning2014$SU08 +learning2014$SU10 + learning2014$SU13 + learning2014$SU16 +
                        learning2014$SU18 + learning2014$SU21 + learning2014$SU24 +
                        learning2014$SU26 + learning2014$SU29 + learning2014$SU32) / 12

# su_lp     SU02 + SU10 + SU18 + SU26
# *Lack of Purpose
# su_um     SU05 + SU13 + SU21 + SU29
# *Unrelated Memorising
# su_sb     SU08 + SU16 + SU24 + SU32
# *Syllabus-boundness

points <- learning2014$Points
dataset <- learning2014[c(57:59, 61:64)]

newdata <- subset(dataset, points > 0, select = c(1:7))

write.csv(newdata)
write.table(newdata)

head(newdata)