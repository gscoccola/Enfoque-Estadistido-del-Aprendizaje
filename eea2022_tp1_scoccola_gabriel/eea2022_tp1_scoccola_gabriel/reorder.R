```{r}
p <- h0 %>%
  mutate(name = fct_relevel(h0$c_verdura,"1 a 3 veces durante los Aºltimos 7 dA-as", "1 vez al dA-a", "2 veces al dA-a", "3 veces al dA-a", "4 a 6 veces durante los Aºltimos 7 dA-as", "4 o mA!s veces al dA-a", "Dato perdido", "No comA- verduras ni hortalizas durante los Aºltimos 7 dA-as")) %>%
  
  ggplot(h0, aes(x=h0$c_verdura))

```