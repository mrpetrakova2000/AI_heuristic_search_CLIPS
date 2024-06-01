# Методы информированного (эвристического) поиска в среде продукционного программирования CLIPS
## Цель
Сравнение метрик информированного (эвристического) поиска с использованием продукционного программирования в среде CLIPS при решении практической задачи.
## Постановка задачи
Рассматривается задача «Головоломка 8-ка». Задана доска с 8 пронумерованными фишками и с одним пустым участком. Фишка, смежная с пустым участком, может быть передвинута на этот участок. Требуется достичь указанного целевого состояния. 
Провести эксперименты использованием эвристических функций h1 и h2.
- h1 – число объектов, находящихся не на своих позициях;
- h2 – суммарное по всем объектам число шагов до целевого положения (манхэттенское расстояние).
## Результаты
По результатам обоих экспериментов видно, что программа при использовании эвристической функции h2 работает гораздо эффективнее и по временной, и по ёмкостной сложностям, чем при использовании h1. Это связано с тем, что эвристика h2 более информативна, чем h1. Состояния могут иметь одинаковое значение по h1, но разное по эвристике h2, более точно отражающей расстояние до целевого состояния.
