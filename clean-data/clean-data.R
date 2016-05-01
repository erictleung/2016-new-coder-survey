# Load in packages
library(dplyr)

# Read in data
read_in_data <- function() {
    # Set path of where data is
    part1Path <- "../raw-data/2016 New Coders Survey Part 1.csv"
    part2Path <- "../raw-data/2016 New Coders Part 2.csv"

    # Read in data
    survey1 <- read.csv(
        file = part1Path,
        stringsAsFactors = FALSE,
        na.strings = "") %>% tbl_df()
    survey2 <- read.csv(
        file = part2Path,
        stringsAsFactors = FALSE,
        na.strings = "") %>% tbl_df()

    list(part1 = survey1, part2 = survey2)
}

# Rename part 1 of survey
rename_part_1 <- function(part1) {
    newPart1 <- part1 %>% rename(
        ID = X.
    ) %>% rename(
        IsSoftwareDev = Are.you.already.working.as.a.software.developer.
    ) %>% rename(
        JobPref = Would.you.prefer.to...
    ) %>% rename(
        JobRoleInterest = Which.one.of.these.roles.are.you.most.interested.in.
    ) %>% rename(
        JobRoleInterestOther = Other
    ) %>% rename(
        JobApplyWhen = When.do.you.plan.to.start.applying.for.developer.jobs.
    ) %>% rename(
        ExpectedEarning = About.how.much.money.do.you.expect.to.earn.per.year.at.your.first.developer.job..in.US.Dollars..
    ) %>% rename(
        JobWherePref = Would.you.prefer.to.work...
    ) %>% rename(
        JobRelocate = Are.you.willing.to.relocate.for.a.job.
    ) %>% rename(
        CodeEventCoffee = coffee.and.codes
    ) %>% rename(
        CodeEventHackathons = hackathons
    ) %>% rename(
        CodeEventConferences = conferences
    ) %>% rename(
        CodeEventNodeSchool = NodeSchool
    ) %>% rename(
        CodeEventRailsBridge = RailsBridge
    ) %>% rename(
        CodeEventStartUpWknd = Startup.Weekend
    ) %>% rename(
        CodeEventWomenCode = Women.Who.Code
    ) %>% rename(
        CodeEventGirlDev = Girl.Develop.It
    ) %>% rename(
        CodeEventNone = None
    ) %>% rename(
        CodeEventOther = Other.1
    ) %>% rename(
        ResourceEdX = EdX
    ) %>% rename(
        ResourceCoursera = Coursera
    ) %>% rename(
        ResourceFCC = Free.Code.Camp
    ) %>% rename(
        ResourceKhanAcademy = Khan.Academy
    ) %>% rename(
        ResourcePluralSight = Code.School..Pluralsight.
    ) %>% rename(
        ResourceCodeacademy = Codecademy
    ) %>% rename(
        ResourceUdacity = Udacity
    ) %>% rename(
        ResourceUdemy = Udemy
    ) %>% rename(
        ResourceCodeWars = Code.Wars
    ) %>% rename(
        ResourceOdinProj = The.Odin.Project
    ) %>% rename(
        ResourceDevTips = DevTips
    ) %>% rename(
        ResourceOther = Other.2
    ) %>% rename(
        PodcastCodeNewbie = Code.Newbie
    ) %>% rename(
        PodcastChangeLog = The.Changelog
    ) %>% rename(
        PodcastSEDaily = Software.Engineering.Daily
    ) %>% rename(
        PodcastJSJabber = JavaScript.Jabber
    ) %>% rename(
        PodcastNone = None.1
    ) %>% rename(
        PodcastOther = Other.3
    ) %>% rename(
        HoursLearning = About.how.many.hours.do.you.spend.learning.each.week.
    ) %>% rename(
        MonthsProgramming = About.how.many.months.have.you.been.programming.for.
    ) %>% rename(
        BootcampYesNo = Have.you.attended.a.full.time.coding.bootcamp.
    ) %>% rename(
        BootcampName = Which.one.
    ) %>% rename(
        BootcampFinish = Have.you.finished.yet.
    ) %>% rename(
        BootcampMonthsAgo = How.many.months.ago.
    ) %>% rename(
        BootcampFullJobAfter = Were.you.able.to.get.a.full.time.developer.job.afterward.
    ) %>% rename(
        BootcampPostSalary = How.much.was.your.salary.
    ) %>% rename(
        BootcampLoan = Did.you.take.out.a.loan.to.pay.for.the.bootcamp.
    ) %>% rename(
        BootcampRecommend = Based.on.your.experience..would.you.recommend.this.bootcamp.to.your.friends.
    ) %>% rename(
        MoneyForLearning = Aside.from.university.tuition..about.how.much.money.have.you.spent.on.learning.to.code.so.far..in.US.dollars..
    ) %>% rename(
        Part1StartTime = Start.Date..UTC.
    ) %>% rename(
        Part1EndTime = Submit.Date..UTC.
    ) %>% rename(
        NetworkID = Network.ID
    )

    newPart1
}

# Simple Title Case Function
# Description:
#    Inteded to use in mutate functions to title case strings
# Input:
#    List of strings or just a string itself
# Output:
#    List of strings or just a string itself
# Example:
#    simple_title_case("hello world")
# Adapted from: http://stackoverflow.com/a/6364905
simple_title_case <- function(x) {
    titleCase <- c()
    for (i in 1:length(x)) {
        s <- strsplit(x[i], " ")[[1]]
        titleS <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                        sep="", collapse=" ")
        titleCase <- c(titleCase, titleS)
    }
    titleCase
}

# Take a range string (e.g. "50-60000") and take average (e.g. "55"). In this
# case, there is a string "50-60000" because there was a "k" at the end I
# removed earlier
average_range_earning <- function(x) {
    avgRange <- c()
    for (i in 1: length(x)) {
        tempRange <- x[i] %>% strsplit("-") %>%
            unlist %>%
            as.numeric %>%
            ifelse(. < 100, . * 1000, .) %>%
            mean %>%
            as.character()
        avgRange <- c(avgRange, tempRange)
    }
    avgRange
}

# Substitution function [WIP]
# Description: Used directly on a dplyr data frame to grep substitute
# Input: dplyr data frame
# Output: dplyr data frame
sub_and_rm <- function(dirtyDat, colName, findStr, replaceStr) {
    varval <- lazyeval::interp(~ gsub(f, r, c),
                               f=findStr,
                               r=replaceStr,
                               c=as.name(colName))

    dirtyIdx <- dirtyDat %>% select_(colName) %>%
        mutate_each(funs(grepl(findStr, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    subDirty <- dirtyDat %>% filter(dirtyIdx) %>%
        mutate_(.dots = setNames(list(varval), colName))

    cleanDat <- dirtyDat %>% filter(!dirtyIdx) %>% bind_rows(subDirty)

    cleanDat
}

# Clean Expected Earnings
clean_expected_earnings <- function(cleanPart1) {
    # Remove dollar signs from expected earnings
    dollarIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl("\\$", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    dollarData <- cleanPart1 %>% filter(dollarIdx) %>%
        mutate(ExpectedEarning = sub("\\$", "", ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!dollarIdx) %>% bind_rows(dollarData)

    # Remove commas from expected earnings
    commaIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl(",", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    commaData <- cleanPart1 %>% filter(commaIdx) %>%
        mutate(ExpectedEarning = sub(",", "", ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!commaIdx) %>% bind_rows(commaData)

    # Remove "k" from expected earnings
    kIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl("k", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    kData <- cleanPart1 %>% filter(kIdx) %>%
        mutate(ExpectedEarning = sub("k", "000", ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!kIdx) %>% bind_rows(kData)

    # Change range of expected earnings into average
    rangeIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl("-", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    rangeData <- cleanPart1 %>% filter(rangeIdx) %>%
        mutate(ExpectedEarning = average_range_earning(ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!rangeIdx) %>% bind_rows(rangeData)

    # Remove period from salaries like 50.000 which should be just 50000
    thousandsIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl("^\\d{2}\\.", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    thousandsData <- cleanPart1 %>% filter(thousandsIdx) %>%
        mutate(ExpectedEarning = sub("\\.", "", ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!thousandsIdx) %>%
        bind_rows(thousandsData)

    # Remove any non-numeric characters
    numericIdx <- cleanPart1 %>% select(ExpectedEarning) %>%
        mutate_each(funs(grepl("[A-Za-z]", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    numericData <- cleanPart1 %>% filter(numericIdx) %>%
        mutate(ExpectedEarning = gsub("[A-Za-z']*", "", ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% filter(!numericIdx) %>%
        bind_rows(numericData)

    # Make all expected earnings less than 100 multiplied by 1000
    below100 <- cleanPart1 %>%
        mutate(ExpectedEarning = as.numeric(ExpectedEarning)) %>%
        filter(ExpectedEarning < 100) %>%
        filter(ExpectedEarning != 0) %>%
        mutate(ExpectedEarning = as.character(ExpectedEarning))
    change100 <- below100 %>%
        mutate(ExpectedEarning = as.numeric(ExpectedEarning)) %>%
        mutate(ExpectedEarning = ExpectedEarning * 1000) %>%
        mutate(ExpectedEarning = as.character(ExpectedEarning))
    cleanPart1 <- cleanPart1 %>% setdiff(below100) %>% bind_rows(change100)

    cleanPart1
}

# Normalize text based on searching list and desired single replacement
# Using non-standard eval in dplyr: http://stackoverflow.com/a/26003971
# More on NSE:
#   https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html
normalize_text <- function(inData, columnName, searchTerms, replaceWith) {
    # Setup dynamic naming of variables later in function
    varval <- lazyeval::interp(~ replaceText, replaceText = replaceWith)

    # Gets indices for rows that need to be changed
    wordIdx <- inData %>% select_(columnName) %>%
        mutate_each(
            funs(grepl(paste(searchTerms, collapse = "|"),
                       .,
                       ignore.case = TRUE)
            )
        ) %>% unlist(use.names = FALSE)

    # Change row values to intended words
    wordData <- inData %>% filter(wordIdx) %>%
        mutate_(.dots = setNames(list(varval), columnName))

    # Combine data back together
    cleanData <- inData %>% filter(!wordIdx) %>% bind_rows(wordData)

    cleanData
}

# Take a range string (e.g. "50-60") and take average (e.g. "55").
average_string_range <- function(x) {
    avgRange <- c()
    for (i in 1:length(x)) {
        tempRange <- x[i] %>% strsplit("-|to") %>%
            unlist %>%
            as.numeric %>%
            mean %>%
            as.character()
        avgRange <- c(avgRange, tempRange)
    }
    avgRange
}

# Remove non-numeric characters and change years to months
years_to_months <- function(x) {
    monthsDat <- c()
    for (i in 1:length(x)) {
        tempMonths <- x[i] %>% gsub("[A-Za-z ]", "", .) %>%
            (function (x) as.numeric(x) * 12) %>%
            as.character()
        monthsDat <- c(monthsDat, tempMonths)
    }
    monthsDat
}

# Remove outliers based on threshold
remove_outlier <- function(x, thres) {
    ifelse(test = x >= thres, yes = as.numeric(NA), no = x)
}

# Clean Part 1 of survey
clean_part_1 <- function(part1) {
    # Job Role Interests

    ## Title case answers for other job interests
    ##  See if I can simplify this by just mutating
    jobRoleOtherYes <- part1 %>% filter(!is.na(JobRoleInterestOther)) %>%
        mutate(JobRoleInterestOther = simple_title_case(JobRoleInterestOther))
    jobRoleOtherNo <- part1 %>% filter(is.na(JobRoleInterestOther))
    cleanPart1 <- jobRoleOtherNo %>% bind_rows(jobRoleOtherYes)

    ## Change uncertain job roles to "Undecided"
    ##  Adapted from: http://stackoverflow.com/a/26766945
    undecidedWords <- c("not sure", "don't know", "not certain",
                        "unsure", "dont know", "undecided",
                        "no preference", "not", "any", "no idea")
    idx <- cleanPart1 %>% select(JobRoleInterestOther) %>%
        mutate_each(
            funs(grepl(paste(undecidedWords, collapse = "|"),
                       .,
                       ignore.case = TRUE)
                 )
            ) %>% unlist(use.names = FALSE)
    undecidedData <- cleanPart1 %>% filter(idx) %>%
        mutate(JobRoleInterestOther = "Undecided")
    cleanPart1 <- cleanPart1 %>% filter(!idx) %>% bind_rows(undecidedData)

    ## Normalize cyber security interests to "Cyber Security"
    ##  e.g. "Cyber security" == "Cybersercurity"
    cyberIdx <- cleanPart1 %>% select(JobRoleInterestOther) %>%
        mutate_each(funs(grepl("cyber", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    cyberData <- cleanPart1 %>% filter(cyberIdx) %>%
        mutate(JobRoleInterestOther = "Cyber Security")
    cleanPart1 <- cleanPart1 %>% filter(!cyberIdx) %>% bind_rows(cyberData)


    # Clean expected earnings column
    cleanPart1 <- clean_expected_earnings(cleanPart1)


    # Code Events Other

    ## Convert coding events to binary/boolean
    codeResources <- cleanPart1 %>%
        select(starts_with("CodeEvent"), -CodeEventOther) %>%
        mutate_each(funs(ifelse(!is.na(.), "1", NA)))
    cleanPart1 <- cleanPart1 %>%
        select(-starts_with("CodeEvent"), CodeEventOther) %>%
        bind_cols(codeResources)

    ## Title case other coding events
    codingEvents <- cleanPart1 %>% filter(!is.na(CodeEventOther)) %>%
        mutate(CodeEventOther = simple_title_case(CodeEventOther))
    codeEventsElse <- cleanPart1 %>% filter(is.na(CodeEventOther))
    cleanPart1 <- codeEventsElse %>% bind_rows(codingEvents)

    ## Normalize variations of "meetup" to "Meetups"
    meetupWords <- c("meetup", "meet")
    cleanPart1 <- normalize_text(inData = cleanPart1,
                   columnName = "CodeEventOther",
                   searchTerms = meetupWords,
                   replaceWith = "Meetup(s)")

    ## Normalize FreeCodeCamp
    fccWords <- c("fcc", "freecodecamp", "free code camp")
    cleanPart1 <- normalize_text(inData = cleanPart1,
                                 columnName = "CodeEventOther",
                                 searchTerms = fccWords,
                                 replaceWith = "Free Code Camp")

    ## Normalize variations of "None"
    nones <- c("non", "none", "haven't", "havent", "not", "nothing",
               "didn't", "n/a", "\bna\b", "never", "nil", "nope")
    nonesIdx <- cleanPart1 %>% select(CodeEventOther) %>%
        mutate_each(
            funs(grepl(paste(nones, collapse = "|"),
                       .,
                       ignore.case = TRUE)
                 )
            ) %>% unlist(use.names = FALSE)
    nonesData <- cleanPart1 %>% filter(nonesIdx) %>%
        mutate(CodeEventOther = NA) %>%
        mutate(CodeEventNone = "1")
    cleanPart1 <- cleanPart1 %>% filter(!nonesIdx) %>% bind_rows(nonesData)

    ## Normalize "bootcamps"
    bootcamps <- c("^bootcamp")
    cleanPart1 <- normalize_text(inData = cleanPart1,
                                 columnName = "CodeEventOther",
                                 searchTerms = bootcamps,
                                 replaceWith = "Bootcamp")

    ## Normalize "Rails Girls"
    railsGirls <- c("^rails? ?girls$")
    cleanPart1 <- normalize_text(inData = cleanPart1,
                                 columnName = "CodeEventOther",
                                 searchTerms = railsGirls,
                                 replaceWith = "Rails Girls")

    ## Normalize "Game Jams" to "Game Jam(s)"
    gameJams <- c("game.*?jams?")
    cleanPart1 <- normalize_text(inData = cleanPart1,
                                 columnName = "CodeEventOther",
                                 searchTerms = gameJams,
                                 replaceWith = "Game Jam(s)")


    # Clean Podcasts Other

    ## Convert Podcasts to binary/boolean
    podcasts <- cleanPart1 %>%
        select(starts_with("Podcast"), -PodcastOther) %>%
        mutate_each(funs(ifelse(!is.na(.), "1", NA)))
    cleanPart1 <- cleanPart1 %>%
        select(-starts_with("Podcast"), PodcastOther) %>%
        bind_cols(podcasts)

    ## Normalize variations of "None" in PodcastOther
    nonePod <- c("non", "none", "haven't", "havent", "not a", "nothing",
               "didn't", "n/a", "\bna\b", "never", "nil", "nope", "not tried")
    nonesPodIdx <- cleanPart1 %>% select(PodcastOther) %>%
        mutate_each(
            funs(grepl(paste(nonePod, collapse = "|"),
                       .,
                       ignore.case = TRUE)
            )
        ) %>% unlist(use.names = FALSE)
    nonesPodData <- cleanPart1 %>% filter(nonesPodIdx) %>%
        mutate(PodcastOther = NA) %>%
        mutate(PodcastNone = "1")
    cleanPart1 <- cleanPart1 %>% filter(!nonesPodIdx) %>%
        bind_rows(nonesPodData)


    # Clean number of hours spent learning

    ## Remove the word "hour(s)"
    hoursIdx <- cleanPart1 %>% select(HoursLearning) %>%
        mutate_each(funs(grepl("hours", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    hoursData <- cleanPart1 %>% filter(hoursIdx) %>%
        mutate(HoursLearning = sub("hours.*", "", HoursLearning))
    cleanPart1 <- cleanPart1 %>% filter(!hoursIdx) %>% bind_rows(hoursData)

    ## Remove hyphen and "to" for ranges of hours
    rangeHrIdx <- cleanPart1 %>% select(HoursLearning) %>%
        mutate_each(funs(grepl("-|to", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    rangeHrData <- cleanPart1 %>% filter(rangeHrIdx) %>%
        mutate(HoursLearning = average_string_range(HoursLearning))
    cleanPart1 <- cleanPart1 %>% filter(!rangeHrIdx) %>%
        bind_rows(rangeHrData)

    ## Remove hours greater than a week
    hoursLearnNumeric <- cleanPart1 %>%
        mutate(HoursLearning = as.numeric(HoursLearning))
    weekHours <- hoursLearnNumeric %>%
        filter(HoursLearning > 24*7) %>%
        mutate(HoursLearning = as.numeric(NA)) # Change to value later
    cleanPart1 <- hoursLearnNumeric %>%
        filter(!(ID %in% weekHours[["ID"]])) %>%
        bind_rows(weekHours) %>%
        mutate(HoursLearning = as.character(HoursLearning))


    # Clean months programming

    ## Change years to months
    yearsProgramIdx <- cleanPart1 %>% select(MonthsProgramming) %>%
        mutate_each(funs(grepl("years", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    yearsProgramData <- cleanPart1 %>% filter(yearsProgramIdx) %>%
        mutate(MonthsProgramming = years_to_months(MonthsProgramming))
    cleanPart1 <- cleanPart1 %>% filter(!yearsProgramIdx) %>%
        bind_rows(yearsProgramData)

    ## Remove non-numeric characters
    cleanPart1 <- cleanPart1 %>% sub_and_rm(colName = "MonthsProgramming",
                              findStr = "[A-Za-z ]",
                              replaceStr = "")

    ## Average the range of months
    avgMonthIdx <- cleanPart1 %>% select(MonthsProgramming) %>%
        mutate_each(funs(grepl("-", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    avgMonthData <- cleanPart1 %>% filter(avgMonthIdx) %>%
        mutate(MonthsProgramming = average_string_range(MonthsProgramming))
    cleanPart1 <- cleanPart1 %>% filter(!avgMonthIdx) %>%
        bind_rows(avgMonthData)


    ## Remove outlier months of programming
    cleanPart1 <- cleanPart1 %>%
        mutate(MonthsProgramming = remove_outlier(MonthsProgramming, 744))


    # Salary post bootcamp
    cleanPart1 <- cleanPart1 %>%
        mutate(BootcampPostSalary = remove_outlier(BootcampPostSalary, 1e10))

    # Money used for learning (not including tuition)

    ## Change variants of "None" to 0
    moneyNone <- c("nil", "none", "not")
    cleanPart1 <- cleanPart1 %>% normalize_text(columnName = "MoneyForLearning",
                                  searchTerms = moneyNone,
                                  replaceWith = "0")

    ## Remove dollar sign and other symbols not including periods
    cleanPart1 <- cleanPart1 %>% sub_and_rm(colName = "MoneyForLearning",
                                            findStr = "\\$|>|<|\\(|\\)",
                                            replaceStr = "")

    ## Remove other text
    cleanPart1 <- cleanPart1 %>% sub_and_rm(colName = "MoneyForLearning",
                                            findStr = "[A-Za-z ]",
                                            replaceStr = "")

    ## Average ranges
    avgLearnIdx <- cleanPart1 %>% select(MoneyForLearning) %>%
        mutate_each(funs(grepl("-", ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    avgLearnData <- cleanPart1 %>% filter(avgLearnIdx) %>%
        mutate(MoneyForLearning = average_string_range(MoneyForLearning))
    cleanPart1 <- cleanPart1 %>% filter(!avgLearnIdx) %>%
        bind_rows(avgLearnData)


    cleanPart1
}

# Rename Part 2 of survey
rename_part_2 <- function(part2) {
    newPart2 <- part2 %>% rename(
        ID = X.
    ) %>% rename(
        Age = How.old.are.you.
    ) %>% rename(
        Gender = What.s.your.gender.
    ) %>% rename(
        CountryCitizen = Which.country.are.you.a.citizen.of.
    ) %>% rename(
        CountryLive = Which.country.do.you.currently.live.in.
    ) %>% rename(
        CityPopulation = About.how.many.people.live.in.your.city.
    ) %>% rename(
        IsEthnicMinority = Are.you.an.ethnic.minority.in.your.country.
    ) %>% rename(
        LanguageAtHome = Which.language.do.you.you.speak.at.home.with.your.family.
    ) %>% rename(
        SchoolDegree = What.s.the.highest.degree.or.level.of.school.you.have.completed.
    ) %>% rename(
        SchoolMajor = What.was.the.main.subject.you.studied.in.university.
    ) %>% rename(
        HasFinancialDependents = Do.you.financially.support.any.dependents.
    ) %>% rename(
        MaritalStatus = What.s.your.marital.status.
    ) %>% rename(
        HasChildren = Do.you.have.children.
    ) %>% rename(
        ChildrenNumber = How.many.children.do.you.have.
    ) %>% rename(
        FinanciallySupporting = Do.you.financially.support.any.elderly.relatives.or.relatives.with.disabilities.
    ) %>% rename(
        DebtAmount = Do.you.have.any.debt.
    ) %>% rename(
        HasHomeMortgage = Do.you.have.a.home.mortgage.
    ) %>% rename(
        HomeMortgageOwe = About.how.much.do.you.owe.on.your.home.mortgage..in.US.Dollars..
    ) %>% rename(
        HasStudentDebt = Do.you.have.student.loan.debt.
    ) %>% rename(
        StudentDebtOwe = About.how.much.do.you.owe.in.student.loans..in.US.Dollars..
    ) %>% rename(
        EmploymentStatus = Regarding.employment.status..are.you.currently...
    ) %>% rename(
        EmploymentStatusOther = Other
    ) %>% rename(
        EmploymentField = Which.field.do.you.work.in.
    ) %>% rename(
        EmploymentFieldOther = Other.1
    ) %>% rename(
        Income = About.how.much.money.did.you.make.last.year..in.US.dollars..
    ) %>% rename(
        CommuteTime = About.how.many.minutes.total.do.you.spend.commuting.to.and.from.work.each.day.
    ) %>% rename(
        IsUnderEmployed = Do.you.consider.yourself.under.employed.
    ) %>% rename(
        HasServedInMilitary = Have.you.served.in.your.country.s.military.before.
    ) %>% rename(
        IsReceiveDiabilitiesBenefits = Do.you.receive.disability.benefits.from.your.government.
    ) %>% rename(
        HasHighSpdInternet = Do.you.have.high.speed.internet.at.your.home.
    ) %>% rename(
        IsSoftwareDev = already_working
    ) %>% rename(
        JobRoleInterest = jobs_interested_in
    ) %>% rename(
        JobPref = want_employment_type
    ) %>% rename(
        ExpectedEarning = expected_earnings
    ) %>% rename(
        JobWherePref = home_or_remote
    ) %>% rename(
        JobRelocate = will_relocate
    ) %>% rename(
        CodeEvent = attended_event_types
    ) %>% rename(
        Resources = learning_resources
    ) %>% rename(
        HoursLearning = hours_learning_week
    ) %>% rename(
        MonthsProgramming = months_learning
    ) %>% rename(
        BootcampYesNo = attend_bootcamp
    ) %>% rename(
        Bootcamp = which_bootcamp
    ) %>% rename(
        BootcampFinish = finished_bootcamp
    ) %>% rename(
        BootcampFullJobAfter = job_after_bootcamp
    ) %>% rename(
        BootcampPostSalary = bootcamp_salary
    ) %>% rename(
        BootcampLoan = loan_for_bootcamp
    ) %>% rename(
        BootcampRecommend = recommend_bootcamp
    ) %>% rename(
        MoneyForLearning = total_spent_learning
    ) %>% rename(
        BootcampMonthsAgo = months_ago_finished
    ) %>% rename(
        Podcast = podcast
    ) %>% rename(
        JobApplyWhen = how_soon_jobhunt
    ) %>% rename(
        Part2StartTime = Start.Date..UTC.
    ) %>% rename(
        Part2EndTime = Submit.Date..UTC.
    ) %>% rename(
        NetworkID = Network.ID
    )

    newPart2
}

# Clean Part 2 of survey
clean_part_2 <- function(part2) {
    cleanPart2 <- part2

    # Helper code to look at data being filtered to be changed
    columnToLookAt <- "MoneyForLearning" # Column name you want to examine
    wordSearch <- c("[^0-9]") %>% # Array of regular expressions to search
        paste(collapse = "|")

    charIdx <- part2 %>% select_(columnToLookAt) %>%
        mutate_each(funs(grepl(wordSearch, ., ignore.case = TRUE))) %>%
        unlist(use.names = FALSE)
    part2 %>% filter(charIdx) %>% select_(columnToLookAt) %>%
        distinct() %>% View
    #############################################

    cleanPart2
}

# Merge Survey Dataset Together
merge_parts <- function(part1, part2) {

}

main <- function() {
    dat <- read_in_data()
    part1 <- rename_part_1(dat$part1)
    part1 <- clean_part_1(part1)

    part2 <- rename_part_2(dat$part2)
    part2 <- clean_part_2(part2)

    write.csv(x = part1,
              file = "2016-FCC-New-Coders-Survey-Data.csv",
              na = "NA")
}

main()