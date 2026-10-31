// MATCH:
const total = sql<number>`sum(${amount})`;
const label = html<string>`<b>${amount}</b>`;
