library(ggplot2)

data <- data.frame(
  response = c("Ja, absolut", 
               "Ja, bis zu einem gewissen Grad", 
               "Nein, ich habe die Erklärungen nicht verstanden", 
               "Ich habe keine Erklärungen bekommen, aber ich hätte gerne welche gehabt", 
               "Ich habe keine Erklärungen gebraucht", 
               "Ich weiss es nicht / Ich erinnere mich nicht"),
  count = c(33, 41, 1, 2, 0, 0)
)
print(data)

top <- max(data$percentage) + 5

ggplot(data, aes(x = percentage , y = reorder(response, count), fill=response)) +
  geom_bar(stat = "identity", width=0.8) +
  geom_text(aes(label = sprintf("%.1f%% (%d)", percentage, count)), hjust = -.2) +  
  labs(
       x = "Verteilung der Antworten", y="Antwortmöglichkeiten") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        axis.text.y = element_text(hjust = 0), 
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none") + 
  xlim(0,top)

