package com.isuwang.soa.transaction.api;

import org.apache.thrift.TException;

/**
 * Soa Transactional ProcessCallback
 *
 * @author craneding
 * @date 16/4/11
 */
public interface GlobalTransactionCallback<T> {

    T doInTransaction() throws TException;

}
