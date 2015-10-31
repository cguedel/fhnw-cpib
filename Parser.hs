-- Jacklist:
-- o for each different Nonterminal - Symbol one Parser
-- o List with all Nonterminal - Symbol
--
-- Do this shit with Pattern matching Jack!

--Nullable

--- fuck das geit nit wo eg wett mache!!!
nullable1 ::  -> Expression -> Bool

nullable1 Epsilon _       = true
nullable1 ts _            = false
nullable1 nts Expression  = nullable1 nts
