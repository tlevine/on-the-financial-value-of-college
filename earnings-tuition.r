library(ggplot2)
library(reshape2)
library(scales)

years.invested <- 65 - 20 # People supposedly retire at 65.
prop.withdrawn <- 0.15 # Withdraw 15% of the initial investment every year, ignoring inflation
stock.market.return <- c(1.06, 1.08)
inflation <- 1.025 # 3% inflation
median.no.college <- 5.8e5

colors <- c(tom = '#fe57a1',
            college = 'grey70',
            work = '#57FD8F',
            retire = '#F1FF57')

earnings.slope <- function(stock.market.return, prop.withdrawn) {
  f <- function(state, year) {
    withdrawal <- prop.withdrawn * inflation ^ year
    new.state <- list(money = state$money * (stock.market.return - withdrawal))
    new.state$withdrawals = state$withdrawals + withdrawal
    new.state
  }
  begin.state <- list(money = 1, withdrawals = 0)
  end.state <- Reduce(f, 1:years.invested, init = begin.state)
  end.state$money + end.state$withdrawals
}

lifetime.earnings <- c(
  engineering = 2e6,
  all.majors = 1.19e6,
  early.childhood.education = 8e5
)

college <- data.frame(
  investment = 'College and work',
  label.x = 1.8e5,
  earnings.intercept = unname(lifetime.earnings),
  earnings.slope = 0,
  label.y = unname(lifetime.earnings),
  full.investment = paste0('College median\n(',
                           gsub('\\.', ' ', names(lifetime.earnings)), ')')
)

retire <- data.frame(
  investment = 'Stock market',
  label.x = 1.5e5,
  earnings.intercept = 0,
  earnings.slope = sapply(min(stock.market.return), function(rate)
                          earnings.slope(rate, prop.withdrawn))
)
retire$label.y <- 0.9 * retire$label.x * retire$earnings.slope
retire$full.investment <- paste0(
  'Stocks\nwith\nwithdrawals\n(', round(100 * (min(stock.market.return) - 1)), '%)')

work <- data.frame(
  investment = 'Stock market and work',
  label.x = 3e4,
  earnings.intercept = 0,
  earnings.slope = sapply(stock.market.return, function(rate)
                          earnings.slope(rate, 0))
)
work$label.y <- 2 * work$label.x * work$earnings.slope
work$full.investment <- paste0(
  'Stocks\nwithout\nwithdrawals\n(', round(100 * (stock.market.return - 1)), '%)')

both <- rbind(college, retire, work)

p.base <- ggplot(both) +
  scale_color_manual(values = unname(c(colors['college'], colors['retire'], colors['work'])),
                     name = 'Investment type') +
  theme_minimal() +
  theme(title = element_text(face = 'bold'),
        plot.background = element_rect(fill = 'black'),
        text = element_text(color = 'white'),
        line = element_line(color = 'white'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous('Cost of college (today dollars)',
                     limits = c(0, 2e5), labels = dollar) +
  scale_y_continuous('Earnings (today dollars)',
                     limits = c(0, 3e6), labels = dollar)

stock.retire.label <- 'Stock investment with withdrawals assumes annual
withdrawals that start at 15% the cost of college and increase
by 2.5% per year. (This serves as a salary.)'

no.college.salary <- round(median.no.college / years.invested / 1000)
stock.work.label <- paste0(
'Stock investment without withdrawals assume that you work
a job that pays your living expenses but does not let you
save money. On median, you can expect a yearly salary starting
at $', no.college.salary, ',000 and increasing with inflation.')

p.predictions <- p.base +
  geom_abline(aes(slope = earnings.slope,
                  color = investment,
                  intercept = earnings.intercept,
                  group = full.investment)) +
  geom_text(aes(x = label.x,
                y = label.y,
                color = investment,
                vjust = 1.2,
                lineheight = .8,
                label = full.investment)) +
  annotate('text', x = 2e5, y = 1e5, label = stock.retire.label,
           color = unname(colors['retire']), hjust = 1, vjust = 0) +
  annotate('text', x = 0, y = 3e6, label = stock.work.label,
           color = unname(colors['work']), hjust = 0, vjust = 1) +
  ggtitle('Predicted return on college and stock market investments')

p.tom.expenses <- p.base + geom_vline(xintercept = 1e5, color = 'white') +
  ggtitle("Tom spent $100,000 on college.")

p.tom.predictions <- p.base + geom_hline(yintercept = 1.19e6, color = 'white') +
  ggtitle('Since Tom has a major in a multidisciplinary science,\nwe predict that he will earn $1.19 million in his life.')

p.tom.both <- p.base +
                geom_vline(xintercept = 1e5, color = 'white') +
                geom_hline(yintercept = 1.15e6, color = 'white') +
                aes(x = 1e5, y = 1.19e6) +
                geom_point(color = colors['tom'], size = 20) +
                geom_text(color = 'white', label = 'Tom') +
                ggtitle("Tom's college expenses and predicted earnings.")

p.tom.comparison <- p.predictions +
  aes(x = 1e5, y = 1.19e6) +
  geom_point(color = colors['tom'], size = 20) +
  geom_text(color = 'white', label = 'Tom') +
  ggtitle("How Tom's investment compares to alternatives")

ppplot <- function(plot, filename) ggsave(filename = filename,
                                          plot = plot,
                                          width = 12, height = 7,
                                         units = 'in', dpi = 200)
ppplot(p.predictions, 'general-predictions.png')
ppplot(p.tom.expenses, 'tom-expenses.png')
ppplot(p.tom.predictions, 'tom-predictions.png')
ppplot(p.tom.both, 'tom-both.png')
ppplot(p.tom.comparison, 'tom-comparison.png')
