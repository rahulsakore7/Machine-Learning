

# Chi square Test for response : timely_response

com_No <- read.csv("R:/Semester 3 Fall 2016/KDD/Project/com_No.csv")

P <-table(com_No$product, com_No$timely_response)
chisq.test(P)   # p-value < 2.2e-16

SP <-table(com_No$sub_product, com_No$timely_response)
chisq.test(SP)    # p-value < 2.2e-16   Chi-squared approximation may be incorrect

I <-table(com_No$issue, com_No$timely_response)
chisq.test(I)   # p-value < 2.2e-16     Chi-squared approximation may be incorrect

SI <-table(com_No$sub_issue, com_No$timely_response)
chisq.test(SI)   # p-value < 2.2e-16

# p-value < 2.2e-16   
chisq.test(table(com_No$date_received, com_No$timely_response))


# p-value < 2.2e-16 f
chisq.test(table(com_No$company_public_response, com_No$timely_response))

# # p-value < 2.2e-16
chisq.test(table(com_No$company, com_No$timely_response))

# p-value = 2.557e-10
chisq.test(table(com_No$state, com_No$timely_response))

#  p-value = 0.000344
chisq.test(table(com_No$zipcode, com_No$timely_response))

# p-value = 2.112e-13
chisq.test(table(com_No$tags, com_No$timely_response))

# p-value < 2.2e-16
chisq.test(table(com_No$consumer_consent_provided, com_No$timely_response))

# p-value < 2.2e-16
chisq.test(table(com_No$submitted_via, com_No$timely_response))


# p-value < 2.2e-16
chisq.test(table(com_No$date_sent_to_company, com_No$timely_response))

# p-value < 2.2e-16
chisq.test(table(com_No$company_response_to_consumer, com_No$timely_response))


# p-value < 2.2e-16
chisq.test(table(com_No$consumer_disputed, com_No$timely_response))


# p-value = 0.4972
chisq.test(table(com_No$complaint_id, com_No$timely_response))




# Chi square Test for response : consumer_disputed


P <-table(com_No$product, com_No$consumer_disputed)
chisq.test(P)   # p-value < 2.2e-16

SP <-table(com_No$sub_product, com_No$consumer_disputed)
chisq.test(SP)    # p-value < 2.2e-16   Chi-squared approximation may be incorrect

I <-table(com_No$issue, com_No$consumer_disputed)
chisq.test(I)   # p-value < 2.2e-16     Chi-squared approximation may be incorrect

SI <-table(com_No$sub_issue, com_No$consumer_disputed)
chisq.test(SI)   # p-value < 2.2e-16

# p-value = 0.0002135  
chisq.test(table(com_No$date_received, com_No$consumer_disputed))


# p-value = 2.599e-07
chisq.test(table(com_No$company_public_response, com_No$consumer_disputed))

# p-value = 4.448e-06
chisq.test(table(com_No$company, com_No$consumer_disputed))

# p-value = 0.0001427
chisq.test(table(com_No$state, com_No$consumer_disputed))

#   p-value = 0.1322
chisq.test(table(com_No$zipcode, com_No$consumer_disputed))

#  p-value = 0.1446
chisq.test(table(com_No$tags, com_No$consumer_disputed))

#  p-value = 8.766e-05
chisq.test(table(com_No$consumer_consent_provided, com_No$consumer_disputed))

# p-value = 2.411e-11
chisq.test(table(com_No$submitted_via, com_No$consumer_disputed))


# p-value = 0.000167
chisq.test(table(com_No$date_sent_to_company, com_No$consumer_disputed))

# p-value < 2.2e-16
chisq.test(table(com_No$company_response_to_consumer, com_No$consumer_disputed))


# p-value < 2.2e-16
chisq.test(table(com_No$timely_response, com_No$consumer_disputed))


# p-value = 0.4972
chisq.test(table(com_No$complaint_id, com_No$consumer_disputed))
















