package com.isuwang.soa.transaction.api;

import com.isuwang.soa.core.TransactionContext;
import com.isuwang.soa.transaction.api.domain.TGlobalTransaction;
import com.isuwang.soa.transaction.api.domain.TGlobalTransactionsStatus;
import com.isuwang.soa.transaction.api.service.GlobalTransactionService;
import org.apache.thrift.TException;

import java.util.Date;

/**
 * Soa Transactional Process Template
 *
 * @author craneding
 * @date 16/4/11
 */
public class GlobalTransactionTemplate {

    public <T> T execute(GlobalTransactionCallback<T> action) throws TException {
        final GlobalTransactionService service = GlobalTransactionFactory.getGlobalTransactionService();
        final TransactionContext context = TransactionContext.Factory.getCurrentInstance();

        boolean success = false;

        TGlobalTransaction globalTransaction = null;
        try {
            globalTransaction = new TGlobalTransaction();
            globalTransaction.setCreatedAt(new Date());
            globalTransaction.setCreatedBy(0);
            globalTransaction.setCurrSequence(0);
            globalTransaction.setStatus(TGlobalTransactionsStatus.New);

            globalTransaction = service.create(globalTransaction);

            context.setCurrentTransactionSequence(0);
            context.setCurrentTransactionId(globalTransaction.getId());

            T result = action.doInTransaction();

            success = true;

            return result;
        } finally {
            if (globalTransaction.getId() != null) {
                service.update(globalTransaction.getId(), context.getCurrentTransactionSequence(), success ? TGlobalTransactionsStatus.Success : TGlobalTransactionsStatus.Fail);
            }
        }
    }

}
