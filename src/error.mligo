(* USER-FACING ERRORS *)

[@inline] let error_CfmmTooLate                                          : int = (2)

[@inline] let error_BuyKitTooLowExpectedKit                              : int = (10)
[@inline] let error_BuyKitPriceFailure                                   : int = (11)
[@inline] let error_BuyKitNoCtokGiven                                    : int = (13)

[@inline] let error_SellKitTooLowExpectedCtok                            : int = (21)
[@inline] let error_SellKitPriceFailure                                  : int = (22)
[@inline] let error_SellKitNoKitGiven                                    : int = (24)

[@inline] let error_AddLiquidityNoCtokGiven                              : int = (30)
[@inline] let error_AddLiquidityNoKitGiven                               : int = (31)
[@inline] let error_AddLiquidityNoLiquidityToBeAdded                     : int = (32)
[@inline] let error_AddLiquidityTooLowLiquidityMinted                    : int = (33)
[@inline] let error_AddLiquidityTooMuchKitRequired                       : int = (34)

[@inline] let error_RemoveLiquidityNoLiquidityBurned                     : int = (41)
[@inline] let error_RemoveLiquidityNoCtokWithdrawnExpected               : int = (42)
[@inline] let error_RemoveLiquidityNoKitWithdrawnExpected                : int = (43)
[@inline] let error_RemoveLiquidityCantWithdrawEnoughCtok                : int = (44)
[@inline] let error_RemoveLiquidityCantWithdrawEnoughKit                 : int = (46)
[@inline] let error_RemoveLiquidityTooMuchLiquidityWithdrawn             : int = (48)

[@inline] let error_LiquidationQueueTooLong                              : int = (50)
[@inline] let error_BidTooLow                                            : int = (51)
[@inline] let error_NoOpenAuction                                        : int = (52)
[@inline] let error_NotAllSlicesClaimed                                  : int = (55)
[@inline] let error_NotAWinningBid                                       : int = (56)

[@inline] let error_InsufficientFunds                                    : int = (60)
[@inline] let error_WithdrawTezFailure                                   : int = (61)
[@inline] let error_MintKitFailure                                       : int = (62)
[@inline] let error_BurrowIsAlreadyActive                                : int = (63)
[@inline] let error_DeactivatingAnOverburrowedBurrow                     : int = (64)
[@inline] let error_DeactivatingAnInactiveBurrow                         : int = (65)
[@inline] let error_DeactivatingWithOutstandingKit                       : int = (66)
[@inline] let error_DeactivatingWithCollateralAtAuctions                 : int = (67)

[@inline] let error_NonExistentBurrow                                    : int = (81)
[@inline] let error_BurrowHasCompletedLiquidation                        : int = (82)
[@inline] let error_UnwantedTezGiven                                     : int = (83)
[@inline] let error_AuthenticationError                                  : int = (84)
[@inline] let error_NotLiquidationCandidate                              : int = (85)
[@inline] let error_UnwarrantedCancellation                              : int = (86)
[@inline] let error_NotACompletedSlice                                   : int = (87)
[@inline] let error_InvalidAvlPtr                                        : int = (88)
[@inline] let error_InvalidLeafPtr                                       : int = (89)
[@inline] let error_BurrowAlreadyExists                                  : int = (90)
[@inline] let error_InvalidLiquidationAuction                            : int = (91)

[@inline] let error_GetContractOptFailure                                : int = (95)

[@inline] let error_GetEntrypointOptFailureBurrowSetDelegate             : int = (106)
[@inline] let error_GetEntrypointOptFailureBurrowTransfer                : int = (108)

[@inline] let error_UnauthorisedCaller                                   : int = (111)
[@inline] let error_GetEntrypointOptFailureReceivePrice                  : int = (112)
[@inline] let error_GetEntrypointOptFailureOracleEntrypoint              : int = (113)
[@inline] let error_GetEntrypointOptFailureFA12Transfer                  : int = (114)
[@inline] let error_GetEntrypointOptFailureFA2Transfer                   : int = (115)
[@inline] let error_GetEntrypointOptFailureCtezGetMarginalPrice          : int = (116)
[@inline] let error_GetEntrypointOptFailureReceiveCtezMarginalPrice      : int = (117)

[@inline] let error_ContractNotDeployed                                  : int = (134)
[@inline] let error_ContractAlreadyDeployed                              : int = (135)

[@inline] let error_UnexpectedParams                                     : int = (140)

[@inline] let error_GetLazyFunctionUnpackFailure                         : int = (150)
[@inline] let error_GetLazyFunctionMissingFunction                       : int = (151)

[@inline] let error_GetEntrypointOptFailureVaultReceiveTez               : int = (160)
[@inline] let error_GetEntrypointOptFailureVaultSendTezToVault           : int = (161)
[@inline] let error_GetEntrypointOptFailureVaultSendTezToContract        : int = (162)
[@inline] let error_GetEntrypointOptFailureVaultSetDelegate              : int = (163)

[@inline] let error_GetEntrypointOptFailureCallVaultReceiveTez           : int = (170)
[@inline] let error_GetEntrypointOptFailureCallVaultSendTezToContract    : int = (171)
[@inline] let error_GetEntrypointOptFailureCallVaultSendTezToVault       : int = (172)
[@inline] let error_GetEntrypointOptFailureCallVaultSetDelegate          : int = (173)

(* INTERNAL ERRORS *)

[@inline] let internalError_NodeTezFoundRoot                             : int = (200)
[@inline] let internalError_NodeHeightFoundRoot                          : int = (201)
[@inline] let internalError_NodeParentFoundRoot                          : int = (202)
[@inline] let internalError_NodeBranchFoundNonBranch                     : int = (203)
[@inline] let internalError_NodeLeafFoundNonLeaf                         : int = (204)
[@inline] let internalError_DerefAvlPtrFoundNonRoot                      : int = (205)
[@inline] let internalError_NodeSetParentFoundRoot                       : int = (206)
[@inline] let internalError_UpdateMatchingChildFoundLeaf                 : int = (207)
[@inline] let internalError_RefRotateLeftCurrentPtrNotBranch             : int = (208)
[@inline] let internalError_RefRotateLeftRightPtrNotBranch               : int = (209)
[@inline] let internalError_RefRotateRightCurrentPtrNotBranch            : int = (210)
[@inline] let internalError_RefRotateRightLeftPtrNotBranch               : int = (211)
[@inline] let internalError_RebalanceHeavyChildNonBranchch               : int = (212)
[@inline] let internalError_BalanceBottomUpFoundLeaf                     : int = (213)
[@inline] let internalError_RefDelParentIsLeaf                           : int = (214)
[@inline] let internalError_AvlDeleteEmptyTreeNonEmptyTree               : int = (215)
[@inline] let internalError_RefPeekFrontFoundRoot                        : int = (216)
[@inline] let internalError_RefSplitPostProcessingInvariantFailed        : int = (217)
[@inline] let internalError_RefSplitRecFoundRoot                         : int = (218)

[@inline] let internalError_ComputeTezToAuctionNegativeResult            : int = (230)

[@inline] let internalError_FractionToTezFloorNegative                   : int = (240)
[@inline] let internalError_FractionToTezFloorZeroDenominator            : int = (241)
[@inline] let internalError_FractionToNatFloorNegative                   : int = (242)
[@inline] let internalError_FractionToNatFloorZeroDenominator            : int = (243)

[@inline] let internalError_CompletedAuctionWithoutOutcome               : int = (250)
[@inline] let internalError_PopCompletedAuctionAuctionNotCompleted       : int = (251)
[@inline] let internalError_PopCompletedAuctionNoCompletedAuction        : int = (252)
[@inline] let internalError_PopCompletedAuctionCompletedAuctionNoOutcome : int = (253)
[@inline] let internalError_OldestCompletedSliceEmptyCompletedAuction    : int = (254)

[@inline] let internalError_CtokSubNegative                              : int = (260)
[@inline] let internalError_CtokOfFractionCeilNegative                   : int = (261)
[@inline] let internalError_CtokOfFractionFloorNegative                  : int = (262)

[@inline] let internalError_KitSubNegative                               : int = (270)
[@inline] let internalError_KitOfFractionCeilNegative                    : int = (271)
[@inline] let internalError_KitOfFractionFloorNegative                   : int = (272)

[@inline] let internalError_LqtSubNegative                               : int = (280)
[@inline] let internalError_LqtOfFractionCeilNegative                    : int = (281)
[@inline] let internalError_LqtOfFractionFloorNegative                   : int = (282)

[@inline] let internalError_PowRecImpossible                             : int = (290) (* NOTE: this really is impossible. *)
[@inline] let internalError_CdivIntIntZeroDenominator                    : int = (291)
[@inline] let internalError_FdivIntIntZeroDenominator                    : int = (292)

[@inline] let internalError_SliceListFromLeafPtrEmptySliceList           : int = (300)
[@inline] let internalError_SliceListRemoveEmptyList                     : int = (301)

[@inline] let internalError_MemGetElementNotFound                        : int = (310)

[@inline] let internalError_TokSubNegative                               : int = (320)
[@inline] let internalError_TokOfFractionCeilNegative                    : int = (321)
[@inline] let internalError_TokOfFractionFloorNegative                   : int = (322)

[@inline] let internalError_DctokGeqCfmmCtok                             : int = (330)
[@inline] let internalError_DkitGeqCfmmCkit                              : int = (331)
