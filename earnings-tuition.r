library(ggplot2)
library(reshape2)
library(scales)

years.invested <- 65 - 20 # People supposedly retire at 65.
prop.withdrawn <- 0.15 # Withdraw 15% of the initial investment every year, ignoring inflation
stock.market.return <- c(1.06, 1.08)
inflation <- 1.025 # 3% inflation

earnings.slope <- function(stock.market.return, prop.withdrawn) {
  f <- function(money, year) money *
         (stock.market.return - (prop.withdrawn * inflation ^ year))
  sum(Reduce(f, 1:years.invested, accumulate = TRUE, init = 1))
}

lifetime.earnings <- c(
  engineering = 2e6,
  all.majors = 1.19e6,
  early.childhood.education = 8e5
)

college <- data.frame(
  investment = 'College',
  label.x = 1.8e5,
  earnings.intercept = unname(lifetime.earnings),
  earnings.slope = 0,
  full.investment = paste0('College median\n(',
                           gsub('\\.', ' ', names(lifetime.earnings)), ')')
)
not.college <- data.frame(
  investment = 'Not College',
  label.x = 1.8e5,
  earnings.intercept = 0,
  earnings.slope = sapply(stock.market.return, function(rate)
                          earnings.slope(rate, prop.withdrawn))
)
not.college$full.investment <- paste0(
  'Stocks\n(', round(100 * (stock.market.return - 1)), '%)')

both <- rbind(college, not.college)

p.base <- ggplot(both) +
  aes(color = investment) +
  theme_minimal() +
  theme(title = element_text(face = 'bold'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous('Cost of college (today dollars)',
                     limits = c(0, 2e5), labels = dollar) +
  scale_y_continuous('Earnings (today dollars)',
                     limits = c(0, 2.5e6), labels = dollar)

stock.label <- 'Stock market lines assume annual withdrawals
that start at 15% the cost of college and increase
by 2.5% per year.'
p.predictions <- p.base +
  geom_abline(aes(slope = earnings.slope,
                  intercept = earnings.intercept,
                  group = full.investment)) +
  geom_text(aes(x = label.x,
                y = earnings.intercept + label.x * earnings.slope,
                vjust = -.2,
                lineheight = .8,
                label = full.investment)) +
  annotate('text', x = 2e5, y = 1e5, label = stock.label,
           hjust = 1, vjust = 0) +
  ggtitle('Predicted return on college and stock market investments')

p.tom.expenses <- p.base + geom_vline(xintercept = 1e5) +
  ggtitle("Tom spent $100,000 on college.")

p.tom.predictions <- p.base + geom_hline(yintercept = 1.19e6) +
  ggtitle('Since Tom has a major in a multidisciplinary science,\nwe predict that he will earn $1.19 million in his life.')

p.tom.both <- p.base +
                geom_vline(xintercept = 1e5) +
                geom_hline(yintercept = 1.15e6) +
                aes(x = 1e5, y = 1.19e6) +
                geom_point(color = '#fe57a1', size = 20) +
                geom_text(color = 'white', label = 'Tom') +
                ggtitle("Tom's college expenses and predicted earnings.")

p.tom.comparison <- p.predictions +
  aes(x = 1e5, y = 1.19e6) +
  geom_point(color = '#fe57a1', size = 20) +
  geom_text(color = 'white', label = 'Tom') +
  ggtitle("How Tom's investment compares to alternatives")

ppplot <- function(plot, filename) ggsave(filename = filename,
                                          plot = plot,
                                          width = 8, height = 6,
                                          units = 'in', dpi = 200)
#ppplot(p.predictions, 'general-predictions.png')
#ppplot(p.tom.expenses, 'tom-expenses.png')
#ppplot(p.tom.predictions, 'tom-predictions.png')
#ppplot(p.tom.both, 'tom-both.png')
#ppplot(p.tom.comparison, 'tom-comparison.png')
